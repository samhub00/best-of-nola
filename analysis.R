# ==============================================================================
# Setup & Libraries
# ==============================================================================
library(tidyverse)
library(jsonlite)
library(lubridate)
library(sf)
library(tigris)
library(scales)
library(viridis)
library(tidytext)

options(tigris_use_cache = TRUE)

# ==============================================================================
# Step 1 & 2: Load Data & Inspect
# ==============================================================================
business <- readRDS("~/best-of-nola/nola_businesses.rds")
reviews  <- readRDS("~/best-of-nola/nola_reviews.rds")

glimpse(business)
glimpse(reviews)

# ==============================================================================
# Step 3: Category Analysis & Selection (Live Jazz & Food Theme)
# ==============================================================================
# 1. Unnest categories for open New Orleans establishments
biz_cats <- business %>%
  filter(
    str_detect(city, regex("New Orleans", ignore_case = TRUE)) | state == "LA",
    is_open == 1,
    !is.na(categories)
  ) %>%
  separate_rows(categories, sep = ",\\s*") %>%
  mutate(categories = str_trim(categories))

# 2. Inspect individual category frequencies
category_counts <- biz_cats %>%
  count(categories, sort = TRUE)

print(head(category_counts, 40), n = 40)

# 3. Define target category buckets
music_categories <- c(
  "Jazz & Blues", "Music Venues", "Piano Bars", "Jazz Lounges", "Performing Arts"
)

food_categories <- c(
  "Restaurants", "Cajun/Creole", "Southern", "Seafood", 
  "American (Traditional)", "American (New)", "Bistros", 
  "Soul Food", "Breakfast & Brunch"
)

# 4. Find candidate businesses with tags in both buckets
candidate_venues <- biz_cats %>%
  group_by(business_id, name, stars, review_count) %>%
  summarize(
    has_music_cat = any(categories %in% music_categories),
    has_food_cat  = any(categories %in% food_categories),
    all_categories = paste(unique(categories), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(has_food_cat, has_music_cat) %>%
  filter(review_count >= 30)

cat("Found", nrow(candidate_venues), "qualifying food & music venues.\n")

# 5. Review text-mining for Jazz & Culinary mentions
jazz_regex <- "\\b(jazz|brass band|trio|sax|saxophone|live music|live band)\\b"
food_regex <- "\\b(gumbo|jambalaya|etouffee|dinner|brunch|entree|po'boy|catfish|crawfish|chef)\\b"

food_music_reviews <- reviews %>%
  filter(business_id %in% candidate_venues$business_id) %>%
  mutate(
    mentions_jazz = str_detect(text, regex(jazz_regex, ignore_case = TRUE)),
    mentions_food = str_detect(text, regex(food_regex, ignore_case = TRUE))
  ) %>%
  group_by(business_id) %>%
  summarize(
    total_reviews_analyzed = n(),
    jazz_mentions = sum(mentions_jazz),
    food_mentions = sum(mentions_food),
    both_mentions = sum(mentions_jazz & mentions_food),
    avg_stars_when_jazz_mentioned = mean(stars[mentions_jazz], na.rm = TRUE),
    .groups = "drop"
  )

# 6. Rank candidates by composite score
ranked_candidates <- candidate_venues %>%
  inner_join(food_music_reviews, by = "business_id") %>%
  mutate(
    jazz_share = jazz_mentions / total_reviews_analyzed,
    food_share = food_mentions / total_reviews_analyzed,
    score = (0.40 * stars) + 
      (0.30 * coalesce(avg_stars_when_jazz_mentioned, stars)) + 
      (0.30 * (log10(both_mentions + 1) / log10(max(both_mentions + 1)))) * 5
  ) %>%
  arrange(desc(score))

print(head(ranked_candidates, 10))

# 7. Form the official 'selected_category_businesses' object with full spatial coordinates
selected_category_businesses <- business %>%
  semi_join(ranked_candidates, by = "business_id") %>%
  left_join(
    ranked_candidates %>% select(business_id, score, jazz_mentions, both_mentions, all_categories),
    by = "business_id"
  )

# ==============================================================================
# Step 4: Join Businesses to Reviews (Filter to Focus Subset)
# ==============================================================================
category_reviews <- reviews %>%
  semi_join(selected_category_businesses, by = "business_id") %>%
  mutate(
    review_date = as_date(date),
    review_year = year(review_date)
  )

cat("Retained", nrow(category_reviews), "reviews across", nrow(selected_category_businesses), "selected businesses.\n")

# ==============================================================================
# Step 5: Official Geographic Boundary Spatial Join
# ==============================================================================
# 1. Download official Louisiana place boundaries
louisiana_places <- tigris::places(
  state = "LA",
  cb = TRUE,
  year = 2024,
  class = "sf"
)

# 2. Convert selected restaurants to spatial points (CRS 4326 = WGS 84)
#    and transform directly to match the places boundary CRS
restaurant_points <- selected_category_businesses %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(st_crs(louisiana_places))

# 3. Spatial join to official Census place boundaries
restaurants_with_place <- restaurant_points %>%
  st_join(
    louisiana_places %>% select(GEOID, place_name = NAME),
    join = st_within,
    left = TRUE
  )

# 4. Check city vs official place_name alignment
mismatch_summary <- restaurants_with_place %>%
  st_drop_geometry() %>%
  count(city, place_name, sort = TRUE)

print(mismatch_summary)

# 5. Filter to restaurants strictly inside New Orleans boundary
nola_official_selection <- restaurants_with_place %>%
  filter(place_name == "New Orleans")

cat("Final confirmed restaurants inside New Orleans bounds:", nrow(nola_official_selection), "\n")

# Extract just the New Orleans polygon
nola_boundary <- louisiana_places %>%
  filter(NAME == "New Orleans")

# Plot restaurants within official New Orleans limits
ggplot() +
  geom_sf(data = nola_boundary, fill = "grey95", color = "grey35") +
  geom_sf(
    data = nola_official_selection,
    aes(color = stars, size = review_count),
    alpha = 0.75
  ) +
  scale_color_viridis_c(option = "C", limits = c(1, 5)) +
  labs(
    title = "Selected Food & Jazz Establishments in New Orleans",
    subtitle = "Filtered to official U.S. Census New Orleans municipal boundaries",
    color = "Yelp Stars",
    size = "Review Count"
  ) +
  theme_minimal()

# ==============================================================================
# Step 6: Choose and Defend a Geographic Area (Compare 2 Boundary Systems)
# ==============================================================================
# --- System A: ZIP Code Tabulation Areas (ZCTAs) ---
la_zctas <- tigris::zctas(
  state = "LA",
  year = 2010,
  class = "sf"
)

# Identify the ZIP code column automatically (usually GEOID20, ZCTA5CE10, or GEOID)
zcta_col_name <- names(la_zctas)[str_detect(names(la_zctas), regex("^(ZCTA5|GEOID)", ignore_case = TRUE))][1]
cat("Detected ZCTA column name:", zcta_col_name, "\n")

la_zctas_clean <- la_zctas %>%
  select(zcta = all_of(zcta_col_name))

jazz_with_zcta <- nola_official_selection %>%
  st_transform(st_crs(la_zctas_clean)) %>%
  st_join(
    la_zctas_clean,
    join = st_within,
    left = TRUE
  )
# ==============================================================================
# Extract & Map the Best Jazz Establishment per Area (ZCTA)
# ==============================================================================

# 1. Extract the #1 highest scoring venue per area (KEEP business_id in select!)
best_jazz_by_area <- jazz_with_zcta %>%
  st_drop_geometry() %>%
  filter(!is.na(zcta)) %>%
  group_by(zcta) %>%
  arrange(desc(score), desc(review_count)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(business_id, zcta, name, stars, review_count, jazz_mentions, score, all_categories) %>%
  arrange(desc(score))

print(best_jazz_by_area)

# 2. Filter spatial points down to winning spots (now business_id exists in y)
best_venues_sf <- jazz_with_zcta %>%
  semi_join(best_jazz_by_area, by = c("zcta", "business_id"))

# 3. Filter ZCTA polygons to New Orleans area for clean mapping
nola_zctas <- la_zctas %>%
  st_filter(nola_boundary %>% st_transform(st_crs(la_zctas)), .predicate = st_intersects)

library(ggrepel)

# 4. Plot #1 best venue in each localized area
venues_bbox <- st_bbox(best_venues_sf)
x_span <- venues_bbox$xmax - venues_bbox$xmin
y_span <- venues_bbox$ymax - venues_bbox$ymin

xlim_zoom <- c(venues_bbox$xmin - 0.15 * x_span, venues_bbox$xmax + 0.15 * x_span)
ylim_zoom <- c(venues_bbox$ymin - 0.15 * y_span, venues_bbox$ymax + 0.15 * y_span)

# 2. Render the zoomed-in map
ggplot() +
  geom_sf(data = nola_zctas, fill = "grey96", color = "grey80", linewidth = 0.4) +
  geom_sf(data = nola_boundary %>% st_transform(st_crs(la_zctas)), fill = NA, color = "grey40", linewidth = 0.7) +
  geom_sf(
    data = best_venues_sf,
    aes(color = score, size = review_count),
    alpha = 0.85
  ) +
  # Add readable labels for the top spots without overlapping
  geom_text_repel(
    data = best_venues_sf,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.2,
    fontface = "bold",
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = 20
  ) +
  # Restrict map viewport directly around the restaurants
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom, expand = FALSE) +
  scale_color_viridis_c(option = "plasma", name = "Composite\nScore") +
  scale_size_continuous(range = c(2.5, 6), name = "Review\nCount") +
  labs(
    title = "Top Food & Jazz Establishment in Each Localized Area",
    subtitle = "Zoomed view of winning venues by New Orleans ZIP Code Tabulation Area",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey90", linetype = "dashed"),
    legend.position = "right"
  )

