library(tidyverse)
library(readxl)
library(lubridate)

marty <- read_excel("cleaned_compiled_marty_data.xlsx")
cs    <- read_excel("clint and serena data cleaned.xlsx")
tt    <- read_excel("tulip tree data combined.xlsx")

make_name <- function(category, type, specific, seconds) {
  category  <- tolower(trimws(ifelse(is.na(category),  "", category)))
  type      <- tolower(trimws(ifelse(is.na(type),      "", type)))
  specific  <- tolower(trimws(ifelse(is.na(specific),  "", specific)))
  is_frozen  <- specific == "frozen"
  is_seconds <- !is.na(seconds) & seconds == TRUE
  case_when(
    is_frozen & type != ""  ~ paste0(category, " - ", type, " - frozen"),
    is_frozen & type == ""  ~ paste0(category, " - frozen"),
    is_seconds & type != "" ~ paste0(category, " - ", type, " - seconds"),
    is_seconds & type == "" ~ paste0(category, " - seconds"),
    type != ""              ~ paste0(category, " - ", type),
    TRUE                    ~ category
  )
}

remap_name <- function(name) {
  case_when(
    startsWith(name, "apples")                                     ~ "apples",
    startsWith(name, "apricots")                                   ~ NA_character_,
    startsWith(name, "elderberry heads")                           ~ NA_character_,
    startsWith(name, "asparagus")                                  ~ "asparagus",
    startsWith(name, "black walnuts")                              ~ NA_character_,
    startsWith(name, "burdock root")                               ~ NA_character_,
    name == "beans - fava tops"                                    ~ NA_character_,
    name %in% c("beans - filet, green",   "beans - filet, purple",
                "beans - dragon tongue",  "beans - green",
                "beans - purple",         "beans - yellow",
                "beans - mixed colors",   "beans - roma")          ~ "beans - fresh variety",
    startsWith(name, "beans")                                      ~ "beans - dry variety",
    name %in% c("beets - baby mixed colors", "beets - badger flame",
                "beets - gold", "beets - mixed colors",
                "beets - white")                                   ~ "beets - mixed colors",
    name %in% c("beets - chioggia", "beets - cylindra",
                "beets - red")                                     ~ "beets - red",
    name %in% c("berries - blueberries",
                "berries - blackberries")                          ~ NA_character_,
    name %in% c("cabbage - caraflex", "cabbage - flat dutch",
                "cabbage - green mini", "cabbage - green mini (tiara)",
                "cabbage - savoy", "cabbage - tendersweet",
                "cabbage - tendersweet - seconds",
                "cabbage - green")                                 ~ "cabbage - green",
    name == "cabbage - brussles sprouts"                           ~ NA_character_,
    name == "cabbage - brussels sprouts"                           ~ NA_character_,
    name %in% c("carrots - rainbow", "carrots - purple",
                "carrots - red",     "carrots - yellow")           ~ "carrots - rainbow",
    name == "carrots - rainbow - seconds"                          ~ NA_character_,
    name == "cherries - tart"                                      ~ NA_character_,
    startsWith(name, "chestnuts")                                  ~ NA_character_,
    startsWith(name, "currants")                                   ~ NA_character_,
    name %in% c("eggplant - indian", "eggplant - japanese")       ~ "eggplant - asian",
    name %in% c("eggplant - dancer",    "eggplant - baby",
                "eggplant - fairytale", "eggplant - nadia / purple",
                "eggplant - italian")                              ~ "eggplant - italian",
    name == "celery - celery leaves"                               ~ NA_character_,
    name == "fennel - leaf"                                        ~ NA_character_,
    startsWith(name, "flowers")                                    ~ NA_character_,
    name %in% c("garlic", "garlic - peeled cloves")               ~ "garlic",
    name %in% c("garlic - paste", "garlic - minced (avocado oil)") ~ NA_character_,
    startsWith(name, "ginger")                                     ~ NA_character_,
    startsWith(name, "grapes")                                     ~ NA_character_,
    name == "greens - amaranth"                                    ~ NA_character_,
    name == "greens - broccoli leaves"                             ~ NA_character_,
    name == "greens - brussel leaves"                              ~ NA_character_,
    name == "greens - baby chinese cabbage"                        ~ NA_character_,
    name == "greens - beet greens"                                 ~ NA_character_,
    name == "greens - chinese broccoli"                            ~ NA_character_,
    name == "greens - chinese cabbage"                             ~ NA_character_,
    name == "greens - grape leaves"                                ~ NA_character_,
    name == "greens - graple leaves"                               ~ NA_character_,
    name == "greens - escarole"                                    ~ NA_character_,
    name == "greens - komatsuna"                                   ~ NA_character_,
    name == "greens - mulberry leaves"                             ~ NA_character_,
    name == "greens - plantain"                                    ~ NA_character_,
    name == "greens - quelites"                                    ~ NA_character_,
    name == "greens - persimmon leaves"                            ~ NA_character_,
    name == "greens - peach leaves"                                ~ NA_character_,
    name == "greens - nettles"                                     ~ NA_character_,
    name == "greens - stir fry mix"                                ~ NA_character_,
    name == "greens - spicy asian mix"                             ~ NA_character_,
    name == "greens - sweet potato"                                ~ NA_character_,
    name == "greens - tatsoi"                                      ~ NA_character_,
    name == "greens - tokyo bekana"                                ~ NA_character_,
    name %in% c("greens - mustard",
                "greens - mixed mustards")                         ~ "greens - mustard",
    startsWith(name, "herbaceous perennial/shrub")                 ~ NA_character_,
    name == "herbs - wild sweet cicely"                            ~ NA_character_,
    name == "herbs - shiso"                                        ~ NA_character_,
    name == "herbs - lemongrass"                                   ~ NA_character_,
    name == "herbs - papalo"                                       ~ NA_character_,
    name == "herbs - chervil"                                      ~ NA_character_,
    name == "herbs - coriander"                                    ~ NA_character_,
    name == "leeks - bulk"                                         ~ "leeks",
    startsWith(name, "horseradish root")                           ~ NA_character_,
    name %in% c("kohlrabi - green", "kohlrabi - kossak",
                "kohlrabi - purple")                               ~ "kohlrabi",
    name == "kohlrabi - green - seconds"                           ~ "kohlrabi - seconds",
    startsWith(name, "medlar fruit")                               ~ NA_character_,
    startsWith(name, "melons")                                     ~ NA_character_,
    name %in% c("mushrooms", "mushrooms - chef's mix",
                "mushrooms - hen of the woods",
                "mushrooms - italian oyster")                      ~ NA_character_,
    name == "onions - scapes"                                      ~ NA_character_,
    name == "onions - tropea - seconds"                            ~ NA_character_,
    name %in% c("onions - tropea", "onions - red")                ~ "onions - red",
    startsWith(name, "peaches")                                    ~ NA_character_,
    startsWith(name, "pears")                                      ~ "pears",
    startsWith(name, "peas")                                       ~ "peas",
    name == "peppers - bell - frozen"                              ~ NA_character_,
    name == "peppers - italian roasting - frozen"                  ~ NA_character_,
    name %in% c("peppers - aji dulce",      "peppers - carmen",
                "peppers - habanada",       "peppers - cubanelle",
                "peppers - escamillo",      "peppers - himo togarashi",
                "peppers - hungarian mild wax",
                "peppers - jimmy nardello", "peppers - melrose",
                "peppers - leysa",          "peppers - lunchbox",
                "peppers - roulette",       "peppers - sweet red italian bullhorn",
                "peppers - sweet mix",      "peppers - italian roasting") ~ "peppers - other sweet/non-spicy",
    name %in% c("peppers - anaheim",        "peppers - banana",
                "peppers - cascabel",       "peppers - carolina reaper",
                "peppers - chilaca",        "peppers - cayenne",
                "peppers - cayenne, bulk",  "peppers - bulk",
                "peppers - chilhaucle negro",
                "peppers - chili de arbol", "peppers - corbaci",
                "peppers - hatch chili",    "peppers - hungarian hot wax",
                "peppers - holy mole",      "peppers - lemon drop",
                "peppers - mixed korean chilis",
                "peppers - portugal",       "peppers - red cherry",
                "peppers - spicy mix",      "peppers - super chili") ~ "peppers - other spicy",
    name == "persimmon - wild"                                     ~ NA_character_,
    startsWith(name, "plums")                                      ~ NA_character_,
    name %in% c("potatoes - mixed - seconds",
                "potatoes - superior")                             ~ NA_character_,
    startsWith(name, "pumpkin")                                    ~ NA_character_,
    name %in% c("radishes - alpine",       "radishes - black spanish",
                "radishes - easter egg",   "radishes - easter egg, bunched",
                "radishes - pods",         "radishes - german giant",
                "radishes - purple",       "radishes - winter mix") ~ "radishes - other snacking",
    name %in% c("radishes - watermelon - seconds",
                "radishes - winter mix - seconds")                 ~ "radishes - other snacking - seconds",
    name == "radishes - french breakfast - seconds"                ~ NA_character_,
    name == "radishes - daikon - seconds"                          ~ NA_character_,
    startsWith(name, "ramps")                                      ~ NA_character_,
    startsWith(name, "rhubarb")                                    ~ "rhubarb",
    startsWith(name, "rutabaga")                                   ~ "rutabaga",
    startsWith(name, "scallions")                                  ~ "scallions",
    startsWith(name, "sumac berries")                              ~ NA_character_,
    name %in% c("summer squash - tromboncino",
                "summer squash - patty pan")                       ~ "summer squash - other",
    name == "winter squash - tromboncino"                          ~ NA_character_,
    name %in% c("summer squash - yellow", "summer squash - yellow zucchini",
                "summer squash - zucchini")                        ~ "summer squash - zucchini",
    startsWith(name, "sunchokes")                                  ~ NA_character_,
    name %in% c("sweet potatoes - japanese",
                "sweet potatoes - white")                          ~ "sweet potatoes - white",
    name %in% c("sweet potatoes - japanese - seconds",
                "sweet potatoes - white - seconds")                ~ "sweet potatoes - white - seconds",
    name %in% c("tomatoes - cocktail - frozen",
                "tomatoes - paste",
                "tomatoes - paste - frozen")                       ~ NA_character_,
    name %in% c("tomatoes - roma - frozen",
                "tomatoes - san marzano - frozen")                 ~ "tomatoes - saucing - frozen",
    name %in% c("tomatoes - san marzano", "tomatoes - saucing",
                "tomatoes - roma")                                 ~ "tomatoes - saucing",
    name %in% c("tomatoes - slicing",
                "tomatoes - red round")                            ~ "tomatoes - red round",
    name == "tomatoes - slicing - seconds"                         ~ "tomatoes - red round - seconds",
    name %in% c("tomatoes - heirloom",
                "tomatoes - ukrainian")                            ~ "tomatoes - heirloom",
    name %in% c("tomatoes - cocktail",
                "tomatoes - cherry/grape")                         ~ "tomatoes - cherry/grape",
    startsWith(name, "turnips")                                    ~ "turnips",
    startsWith(name, "walnuts")                                    ~ NA_character_,
    startsWith(name, "watermelon")                                 ~ NA_character_,
    name %in% c("winter squash - acorn",        "winter squash - autumn frost",
                "winter squash - sweet dumpling","winter squash - white acorn")  ~ "winter squash - acorn",
    name %in% c("winter squash - butternut",    "winter squash - candy roaster",
                "winter squash - honeynut",      "winter squash - honey patch")  ~ "winter squash - butternut",
    name %in% c("winter squash - honeynut - seconds",
                "winter squash - butternut - seconds")             ~ "winter squash - butternut - seconds",
    name %in% c("winter squash - lodi",              "winter squash - marina de chioggia",
                "winter squash - cushaw",             "winter squash - baby blue hubbard",
                "winter squash - tetsukabuto",        "winter squash - black futsu",
                "winter squash - pink banana",        "winter squash - pumpkin")  ~ "winter squash - pumpkin",
    TRUE                                                           ~ name
  )
}

marty <- marty %>%
  mutate(
    date           = as.Date(date),
    month_num      = month(date),
    total_quantity = as.numeric(total_quantity),
    total_price    = as.numeric(total_price),
    price_per      = ifelse(total_quantity > 0, total_price / total_quantity, NA),
    cleaned_product_name = remap_name(make_name(product_category, product_type,
                                                product_specific, seconds))
  )

cs <- cs %>%
  mutate(
    date           = as.Date(date),
    month_num      = month(date),
    price_per      = as.numeric(gsub("\\$", "", price_per)),
    total_quantity = as.numeric(total_quantity),
    cleaned_product_name = remap_name(make_name(product_category, product_type,
                                                product_specific, seconds))
  )

tt <- tt %>%
  mutate(
    date           = as.Date(as.numeric(date), origin = "1899-12-30"),
    month_num      = month(date),
    total_quantity = as.numeric(total_quantity),
    cleaned_product_name = remap_name(make_name(product_category, product_type,
                                                product_specific, seconds))
  )

supply_monthly <- marty %>%
  filter(!is.na(cleaned_product_name)) %>%
  group_by(cleaned_product_name, product_category, month_num) %>%
  summarise(supply    = sum(total_quantity, na.rm=TRUE),
            avg_price = mean(price_per, na.rm=TRUE), .groups="drop")

demand_monthly <- bind_rows(
  cs %>% filter(!is.na(cleaned_product_name)) %>%
    group_by(cleaned_product_name, month_num) %>%
    summarise(demand = sum(total_quantity, na.rm=TRUE), .groups="drop"),
  tt %>% filter(!is.na(cleaned_product_name)) %>%
    group_by(cleaned_product_name, month_num) %>%
    summarise(demand = sum(total_quantity, na.rm=TRUE), .groups="drop")
) %>%
  group_by(cleaned_product_name, month_num) %>%
  summarise(demand = sum(demand, na.rm=TRUE), .groups="drop")

full_grid <- supply_monthly %>%
  left_join(demand_monthly, by = c("cleaned_product_name","month_num")) %>%
  mutate(demand = replace_na(demand, 0))

# ── Unit label per product ─────────────────────────────────────────────────
unit_lookup <- marty %>%
  filter(!is.na(cleaned_product_name), !is.na(weight)) %>%
  mutate(weight_norm = tolower(trimws(weight)),
         weight_norm = case_when(
           weight_norm %in% c("lbs", "lb") ~ "lb",
           weight_norm == "gal"             ~ "/gal",
           TRUE                             ~ "ea"
         )) %>%
  group_by(cleaned_product_name, weight_norm) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cleaned_product_name) %>%
  summarise(
    unit_label = {
      types <- weight_norm[order(-n)]
      unique_types <- unique(types)
      if (length(unique_types) == 1 && unique_types == "lb")        "lb"
      else if (length(unique_types) == 1 && unique_types == "/gal") "/gal"
      else if (!any(unique_types == "ea"))                           types[1]
      else                                                           "unit"
    },
    .groups = "drop"
  )

full_grid <- full_grid %>%
  left_join(unit_lookup, by = "cleaned_product_name") %>%
  mutate(unit_label = replace_na(unit_label, "unit"))

MONTHS <- c("Jan","Feb","Mar","Apr","May","Jun",
            "Jul","Aug","Sep","Oct","Nov","Dec")

lines <- c()
add <- function(...) { lines <<- c(lines, paste0(...)) }

add('<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8">')
add('<meta name="viewport" content="width=device-width, initial-scale=1.0">')
add('<title>12-Month Seasonality Guide</title>')
add('<style>')
add('*{box-sizing:border-box;margin:0;padding:0;}')
add('body{background:#f5f0e8;font-family:Segoe UI,Arial,sans-serif;}')
add('.page-header{background:#1e3a2a;color:#f0e8d0;padding:32px 48px 24px;border-bottom:3px solid #c4721a;display:flex;justify-content:space-between;align-items:flex-start;}')
add('.page-header-left{flex:1;}')
add('.page-header-credit{flex-shrink:0;border:1px solid rgba(196,114,26,0.6);border-radius:6px;padding:10px 16px;font-size:12px;color:#d4c8a0;line-height:1.6;text-align:right;max-width:220px;margin-left:32px;margin-top:4px;background:rgba(0,0,0,0.15);}')
add('.page-header h1{font-size:36px;margin:0 0 6px;}')
add('.page-header p{font-size:13px;color:#b0a888;margin:0;}')
add('.controls{background:#faf6ee;border-bottom:1px solid #d4c8a8;padding:12px 48px;position:sticky;top:0;z-index:100;box-shadow:0 2px 8px rgba(0,0,0,0.06);display:flex;align-items:center;gap:16px;flex-wrap:wrap;}')
add('.controls label{font-size:11px;color:#8a7d60;text-transform:uppercase;letter-spacing:1px;}')
add('.controls select{padding:4px 12px;border-radius:16px;border:1.5px solid #d4c8a8;font-size:12px;background:#fff;color:#1a1a14;cursor:pointer;}')
add('.legend{display:flex;gap:20px;flex-wrap:wrap;font-size:11px;color:#6a6050;margin-left:auto;}')
add('.legend-item{display:flex;align-items:center;gap:6px;}')
add('.legend-swatch{width:14px;height:14px;border-radius:2px;flex-shrink:0;}')
add('.legend-dot-ex{width:9px;height:9px;border-radius:50%;background:#0d9488;flex-shrink:0;}')
add('.main{max-width:1300px;margin:0 auto;padding:24px 48px 64px;}')
add('.section-header{font-size:22px;font-weight:700;color:#1e3a2a;border-bottom:2px solid #1e3a2a;padding-bottom:8px;margin:32px 0 12px;}')
add('.cards-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(340px,1fr));gap:16px;}')
add('.variety-card{background:#faf6ee;border:1px solid #d4c8a8;border-radius:6px;padding:14px;padding-bottom:10px;position:relative;min-height:250px;overflow:hidden;}')
add('.card-name{font-weight:700;font-size:14px;color:#1a1a14;margin-bottom:2px;}')
add('.card-cat{font-size:10px;color:#8a7d60;text-transform:uppercase;letter-spacing:1px;margin-bottom:12px;}')
add('.chart-area{width:100%;margin-top:4px;}')
add('.bars-row{display:flex;width:100%;height:100px;align-items:flex-end;gap:1px;overflow:visible;}')
add('.bar-wrap{flex:1;display:flex;align-items:flex-end;height:100%;}')
add('.bar-fill{width:100%;border-radius:2px 2px 0 0;min-height:2px;cursor:crosshair;}')
add('.price-row{display:flex;width:100%;height:8px;align-items:stretch;gap:1px;margin-top:3px;}')
add('.price-cell{flex:1;border-radius:1px;cursor:crosshair;}')
add('.price-label-row{display:flex;width:100%;gap:1px;margin-top:2px;margin-bottom:3px;}')
add('.price-lbl{flex:1;text-align:center;font-size:7px;color:#b07830;}')
add('.labels-row{display:flex;width:100%;gap:1px;margin-top:4px;border-top:1px dashed #e0d8c8;padding-top:3px;}')
add('.month-lbl{flex:1;text-align:center;font-size:8px;color:#aaa;}')
add('.dots-row{display:flex;width:100%;height:14px;align-items:center;gap:1px;margin-top:3px;}')
add('.dot-cell{flex:1;display:flex;justify-content:center;align-items:center;cursor:crosshair;}')
add('.demand-dot{background:#0d9488;border-radius:50%;}')
add('.tt{position:fixed;background:#1e3a2a;color:#f0e8d0;padding:10px 14px;border-radius:6px;font-size:12px;line-height:1.6;pointer-events:none;display:none;z-index:9999;box-shadow:0 4px 16px rgba(0,0,0,0.3);max-width:220px;border:1px solid #c4721a;}')
add('.tt-name{font-weight:700;font-size:13px;margin-bottom:4px;color:#f5d89a;}')
add('.tt-row{display:flex;justify-content:space-between;gap:16px;}')
add('.tt-label{color:#b0c8a0;}')
add('.tt-val{font-weight:600;color:#f0e8d0;}')
add('.tt-demand{color:#0d9488;}')
add('</style></head><body>')

add('<div class="page-header">')
add('  <div class="page-header-left">')
add('    <h1>12-Month Seasonality Guide</h1>')
add('    <p>Supply volume &middot; Price intensity (amber bar color) &middot; Consumer demand (teal dots)</p>')
add('  </div>')
add('  <div class="page-header-credit">')
add('    Created by<br><strong>Jungeun Lee</strong><br>for<br><strong>FarmFED Cooperative</strong>')
add('  </div>')
add('</div>')

add('<div class="controls">')
add('  <label>Category:</label>')
add('  <select id="catFilter" onchange="filterCards(this.value)">')
add('    <option value="all">All Categories</option>')
add('    <option value="__demand__">&#x2605; With sales/demand data</option>')
add('    <option value="__seconds__">&#x2605; Seconds</option>')
add('  </select>')
add('  <span class="legend">')
add('    <span class="legend-item"><span class="legend-swatch" style="background:linear-gradient(to top,#1e3a2a,#4a8c5c)"></span>Supply volume (taller = higher volume)</span>')
add('    <span class="legend-item"><span class="legend-swatch" style="background:linear-gradient(to right,#fde8b0,#c4721a)"></span>Price intensity (darker = higher price per unit)</span>')
add('    <span class="legend-item"><span class="legend-dot-ex"></span>Consumer demand (larger = greater demand)</span>')
add('  </span>')
add('</div>')

add('<div class="tt" id="tooltip">')
add('  <div class="tt-name" id="tt-name"></div>')
add('  <div class="tt-row"><span class="tt-label">Month</span><span class="tt-val" id="tt-month"></span></div>')
add('  <div class="tt-row"><span class="tt-label">Supply</span><span class="tt-val" id="tt-supply"></span></div>')
add('  <div class="tt-row"><span class="tt-label">Avg Price</span><span class="tt-val" id="tt-price"></span></div>')
add('  <div class="tt-row"><span class="tt-label">Demand</span><span class="tt-demand" id="tt-demand"></span></div>')
add('</div>')

add('<script>')
add('function filterCards(cat){')
add('  document.querySelectorAll(".card-slot").forEach(el=>{')
add('    const card=el.querySelector(".variety-card");')
add('    let show=false;')
add('    if(cat==="all") show=true;')
add('    else if(cat==="__demand__") show=card.dataset.hasDemand==="1";')
add('    else if(cat==="__seconds__") show=card.dataset.isSeconds==="1";')
add('    else show=card.dataset.category===cat;')
add('    el.style.display=show?"":"none";')
add('  });')
add('  document.querySelectorAll(".section-header").forEach(el=>{')
add('    const grid=el.nextElementSibling;if(!grid)return;')
add('    const any=[...grid.querySelectorAll(".card-slot")].some(s=>s.style.display!=="none");')
add('    el.style.display=any?"":"none";grid.style.display=any?"":"none";')
add('  });')
add('}')
add('window.addEventListener("DOMContentLoaded",()=>{')
add('  const sel=document.getElementById("catFilter");')
add('  const cats=new Set();')
add('  document.querySelectorAll(".variety-card").forEach(el=>cats.add(el.dataset.category));')
add('  [...cats].sort().forEach(c=>{const o=document.createElement("option");o.value=c;o.textContent=c;sel.appendChild(o);});')
add('});')
add('const tt=document.getElementById("tooltip");')
add('document.addEventListener("mousemove",e=>{')
add('  const col=e.target.closest("[data-month]");')
add('  if(!col){tt.style.display="none";return;}')
add('  const card=col.closest(".variety-card");')
add('  if(!card){tt.style.display="none";return;}')
add('  document.getElementById("tt-name").textContent=card.dataset.name;')
add('  document.getElementById("tt-month").textContent=col.dataset.month;')
add('  const unit=card.dataset.unit||"unit";')
add('  const unitLabel=unit==="lb"?"lbs":unit==="/gal"?"gal":unit==="unit"?"units":unit;')
add('  document.getElementById("tt-supply").textContent=parseInt(col.dataset.supply).toLocaleString()+" "+unitLabel;')
add('  document.getElementById("tt-price").textContent=col.dataset.price>0?"$"+parseFloat(col.dataset.price).toFixed(2)+"/"+card.dataset.unit:"—";')
add('  document.getElementById("tt-demand").textContent=parseInt(col.dataset.demand)>0?parseInt(col.dataset.demand).toLocaleString()+" units":"no data";')
add('  const x=e.clientX,y=e.clientY,w=window.innerWidth;')
add('  tt.style.display="block";')
add('  tt.style.left=(x+220>w?x-230:x+14)+"px";')
add('  tt.style.top=(y-10)+"px";')
add('});')
add('</script>')

add('<div class="main">')

custom_order <- list(
  eggplant = c("eggplant - asian", "eggplant - japanese - seconds",
               "eggplant - italian"),
  radishes = c("radishes - cherry belle", "radishes - french breakfast",
               "radishes - watermelon",   "radishes - daikon",
               "radishes - other snacking",
               "radishes - other snacking - seconds"),
  `summer squash` = c("summer squash - zucchini", "summer squash - other"),
  `sweet potatoes` = c("sweet potatoes - orange", "sweet potatoes - orange - seconds",
                       "sweet potatoes - white",  "sweet potatoes - white - seconds"),
  tomatoes = c("tomatoes - cherry/grape",          "tomatoes - cherry/grape - frozen",
               "tomatoes - cherry/grape - seconds",
               "tomatoes - red round",              "tomatoes - red round - frozen",
               "tomatoes - red round - seconds",
               "tomatoes - saucing",                "tomatoes - saucing - frozen",
               "tomatoes - heirloom",               "tomatoes - heirloom - frozen",
               "tomatoes - heirloom - seconds")
)

two_col_cats <- c("sweet potatoes", "beets", "mushrooms")

get_varieties <- function(catname, available) {
  key <- tolower(catname)
  if (key %in% names(custom_order)) {
    pinned <- intersect(custom_order[[key]], available)
    rest   <- sort(setdiff(available, pinned))
    c(pinned, rest)
  } else {
    sort(available)
  }
}

add('<style>.two-col-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:16px;}</style>')

categories <- full_grid %>%
  distinct(product_category) %>%
  arrange(product_category) %>%
  pull(product_category)

for (catname in categories) {
  available <- full_grid %>%
    filter(product_category == catname) %>%
    pull(cleaned_product_name) %>%
    unique()
  
  cat_varieties <- get_varieties(catname, available)
  grid_class    <- if (tolower(catname) %in% two_col_cats) "two-col-grid" else "cards-grid"
  
  add(sprintf('<div class="section-header">%s</div>', catname))
  add(sprintf('<div class="%s">', grid_class))
  
  for (v in cat_varieties) {
    vdata <- data.frame(month_num = 1:12) %>%
      left_join(full_grid %>% filter(cleaned_product_name == v), by = "month_num") %>%
      mutate(
        supply    = replace_na(supply,    0),
        avg_price = replace_na(avg_price, 0),
        demand    = replace_na(demand,    0)
      )
    
    max_supply <- max(vdata$supply,    1)
    max_price  <- max(vdata$avg_price, 1)
    max_demand <- max(vdata$demand,    1)
    
    has_demand <- if (sum(vdata$demand) > 0) "1" else "0"
    is_seconds <- if (grepl("seconds", v)) "1" else "0"
    unit_label <- full_grid$unit_label[full_grid$cleaned_product_name == v][1]
    if (is.na(unit_label)) unit_label <- "unit"
    
    bars <- price_cells <- price_labels <- labels <- dots <- ""
    
    for (i in 1:12) {
      s <- vdata$supply[i]
      p <- vdata$avg_price[i]
      d <- vdata$demand[i]
      
      # Supply bar: log-scaled height, dark→light green by relative price
      h  <- if (s > 0) max(3, round(log1p(s) / log1p(max_supply) * 100)) else 0
      pr <- if (max_price > 0) p / max_price else 0
      supply_col <- sprintf("rgb(%d,%d,%d)",
                            round(30+(1-pr)*120), round(90+(1-pr)*90), round(42+(1-pr)*20))
      
      # Price intensity cell: fixed 8px, pale yellow→deep amber
      price_col <- if (s > 0 && p > 0) {
        sprintf("rgb(%d,%d,%d)",
                round(253 - pr*57),
                round(232 - pr*118),
                round(176 - pr*150))
      } else {
        "rgba(0,0,0,0.06)"
      }
      
      # Demand dot
      ds <- if (d > 0) max(4, min(13, round(d / max_demand * 13))) else 0
      
      data_attrs <- sprintf(
        'data-month="%s" data-supply="%d" data-price="%.2f" data-demand="%d"',
        MONTHS[i], round(s), p, round(d))
      
      bars <- paste0(bars, sprintf(
        '<div class="bar-wrap" %s><div class="bar-fill" style="height:%dpx;background:%s;"></div></div>',
        data_attrs, h, supply_col))
      
      price_cells <- paste0(price_cells, sprintf(
        '<div class="price-cell" style="background:%s;" %s></div>',
        price_col, data_attrs))
      
      price_labels <- paste0(price_labels, sprintf(
        '<div class="price-lbl">%s</div>',
        if (p > 0) sprintf("$%.2f", p) else ""))
      
      labels <- paste0(labels, sprintf('<div class="month-lbl">%s</div>', MONTHS[i]))
      
      dots <- paste0(dots, sprintf('<div class="dot-cell" %s>%s</div>',
                                   data_attrs,
                                   if (ds > 0) sprintf('<div class="demand-dot" style="width:%dpx;height:%dpx;"></div>', ds, ds) else ""))
    }
    
    add(sprintf(
      '<div class="card-slot">
  <div class="variety-card" data-category="%s" data-name="%s" data-unit="%s" data-has-demand="%s" data-is-seconds="%s">
    <div class="card-name">%s</div>
    <div class="card-cat">%s</div>
    <div class="chart-area">
      <div class="bars-row">%s</div>
      <div class="price-row">%s</div>
      <div class="price-label-row">%s</div>
      <div class="labels-row">%s</div>
      <div class="dots-row">%s</div>
    </div>
  </div>
</div>',
      catname, v, unit_label, has_demand, is_seconds,
      v, catname, bars, price_cells, price_labels, labels, dots))
  }
  
  add('</div>')
}

add('</div>')
add('</body></html>')

writeLines(lines, "almanac.html")
browseURL("almanac.html")