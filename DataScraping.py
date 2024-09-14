#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: michellehajduk
"""
# Adapted from original code by [scrapfly]
# Original source: [https://scrapfly.io/blog/how-to-scrape-idealista/]
# Modifications: Added nest_asyncio as well as main and write_to_csv function
#                Removed currency type and last updated fields, 
#                Added province and autonomous community columns, 
#                Removed maximum pagination restriction


import re
import asyncio
import json
import math
import csv
from pathlib import Path
from typing import TypedDict, List, Dict
from urllib.parse import urljoin
from collections import defaultdict
from scrapfly import ScrapeApiResponse, ScrapeConfig, ScrapflyClient
import nest_asyncio


nest_asyncio.apply()

scrapfly = ScrapflyClient(key="scp-live-2a12c883f6f548c5ba8d81d97fdf762b", max_concurrency=2)

class PropertyDetails(TypedDict):
    url: str
    title: str
    location: str
    price: int
    features: Dict[str, List[str]]
    autonomous_community: str
    province: str

    
def parse_property(response: ScrapeApiResponse) -> PropertyDetails:
    """Extract data from an Idealista.com property page."""
    # Parse the HTML tree from the response:
    page = response.selector
    get_text = lambda query: page.css(query).get("").strip()
    get_all_text = lambda query: page.css(query).getall()

    info = {}
    # Metadata
    info["url"] = str(response.context["url"])

    # Main information
    info['title'] = get_text("h1 .main-info__title-main::text")
    info['location'] = get_text(".main-info__title-minor::text")
    info['price'] = int(get_text(".info-data-price span::text").replace(",", ""))

    # Features
    info["features"] = {}
    for feature_section in page.css(".details-property-h2"):
        section_label = feature_section.xpath("text()").get()
        feature_items = feature_section.xpath("following-sibling::div[1]//li")
        info["features"][section_label] = [
            ''.join(item.xpath(".//text()").getall()).strip() for item in feature_items
        ]
        
    # To be adapted manually    
    info['autonomous_community'] = "Madrid Autonomous Community"
    info['province'] = "Madrid Province"

    return info


async def scrape_properties(url_list: List[str]) -> List[PropertyDetails]:
    """Fetch property data from Idealista.com."""
    properties_data = []
    tasks = [ScrapeConfig(url, asp=True, country="ES") for url in url_list]
    async for res in scrapfly.concurrent_scrape(tasks):
        if res.upstream_status_code != 200:
            print(f"Error fetching: {res.context['url']}")
            continue
        properties_data.append(parse_property(res))
    return properties_data

def parse_search(response: ScrapeApiResponse) -> List[str]:
    """Extract property listing URLs from a search results page."""
    page = response.selector
    property_links = page.css("article.item .item-link::attr(href)").getall()
    return [urljoin(str(response.context["url"]), link) for link in property_links]


async def scrape_search(url: str, paginate=True, max_pages: int = None) -> List[str]:
    """
    Fetch property URLs from search results.
    """
    initial_page = await scrapfly.async_scrape(ScrapeConfig(url, asp=True, country="ES"))
    listing_urls = parse_search(initial_page)
    
    if not paginate:
        return listing_urls
    
    # Calculate the total number of pages
    total_listings = initial_page.selector.css("h1#h1-container").re(": (.+) houses")[0]
    max_page_count = math.ceil(int(total_listings.replace(",", "")) / 30)
    
    # Limit pagination based on the provided max_pages argument
    if max_pages and max_pages < max_page_count:
        max_page_count = max_pages

    # Create tasks for scraping all pages
    task_list = [
        ScrapeConfig(initial_page.context["url"] + f"pagina-{page}.htm", asp=True, country="ES")
        for page in range(2, max_page_count + 1)
    ]
    
    # Scrape remaining pages
    async for res in scrapfly.concurrent_scrape(task_list):
        listing_urls.extend(parse_search(res))
    
    return listing_urls


async def main():
    max_pages = 1
    # PHASE 1: Enter first page URL of listings in province 
    search_url ="https://www.idealista.com/en/alquiler-viviendas/madrid/corredor-del-henares/"
    
    # PHASE 2: Get list of property URLS from search url
    listing_urls = await scrape_search(
        url=search_url,
        max_pages=max_pages
    )
    
    # PHASE 3: Extract property information from each URL
    properties_data = await scrape_properties(listing_urls)
    return properties_data


def write_to_csv(properties_data: List[dict], filename: str):
    """
    Write scraped properties data into a CSV file.
    """
    fieldnames = properties_data[0].keys()
    with open(filename, 'w', newline='') as output_file:
        csv_writer = csv.DictWriter(output_file, fieldnames=fieldnames)
        csv_writer.writeheader()
        csv_writer.writerows(properties)


### RUN THE SCRAPER ###

if __name__ == "__main__":
    properties = asyncio.get_event_loop().run_until_complete(main())
    write_to_csv(properties, 'properties_Madrid_CorredorDelHenares.csv')

### URLS THAT WERE SCRAPED ###
"""
    "https://www.idealista.com/en/alquiler-viviendas/madrid/corredor-del-henares/",
    "https://www.idealista.com/en/alquiler-viviendas/madrid-madrid/",
    "https://www.idealista.com/en/alquiler-viviendas/madrid/zona-noroeste/",
    "https://www.idealista.com/en/alquiler-viviendas/madrid/zona-norte/",
    "https://www.idealista.com/en/alquiler-viviendas/madrid/zona-sur/",
    "https://www.idealista.com/en/alquiler-viviendas/madrid/zona-sureste/",
    "https://www.idealista.com/en/alquiler-viviendas/madrid/zona-suroeste/",
    "https://www.idealista.com/en/alquiler-viviendas/toledo-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/avila-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/segovia-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/guadalajara-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/cuenca-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/ciudad-real-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/badajoz-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/caceres-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/salamanca-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/valladolid-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/burgos-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/palencia-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/la-rioja/",
    "https://www.idealista.com/en/alquiler-viviendas/alava/",
    "https://www.idealista.com/en/alquiler-viviendas/guipuzcoa/",
    "https://www.idealista.com/en/alquiler-viviendas/vizcaya/",
    "https://www.idealista.com/en/alquiler-viviendas/las-palmas/fuerteventura/",
    "https://www.idealista.com/en/alquiler-viviendas/las-palmas/gran-canaria/",
    "https://www.idealista.com/en/alquiler-viviendas/las-palmas/lanzarote/",
    "https://www.idealista.com/en/alquiler-viviendas/leon-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/soria-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/asturias/", 
    "https://www.idealista.com/en/alquiler-viviendas/lugo-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/a-coruna-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/pontevedra-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/ourense-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/zamora-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/huelva-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/sevilla-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/cadiz-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/ceuta-ceuta/",
    "https://www.idealista.com/en/alquiler-viviendas/melilla-melilla/",
    "https://www.idealista.com/en/alquiler-viviendas/malaga/area-de-antequera/",
    "https://www.idealista.com/en/alquiler-viviendas/malaga-malaga/",
    "https://www.idealista.com/en/alquiler-viviendas/malaga/costa-del-sol-occidental-area-de-estepona/",
    "https://www.idealista.com/en/alquiler-viviendas/malaga/costa-del-sol-occidental-area-de-benalmadena-costa/",
    "https://www.idealista.com/en/alquiler-viviendas/malaga/costa-del-sol-occidental-area-de-marbella/",
    "https://www.idealista.com/en/alquiler-viviendas/malaga/costa-del-sol-oriental-axarquia/",
    "https://www.idealista.com/en/alquiler-viviendas/malaga/serrania-de-ronda/",
    "https://www.idealista.com/en/alquiler-viviendas/malaga/valle-de-guadalhorce/",
    "https://www.idealista.com/en/alquiler-viviendas/cordoba-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/granada-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/almeria-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/jaen-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/murcia-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/alicante/",
    "https://www.idealista.com/en/alquiler-viviendas/albacete-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/valencia-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/castellon/",
    "https://www.idealista.com/en/alquiler-viviendas/teruel-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/tarragona-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/zaragoza-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/navarra/",
    "https://www.idealista.com/en/alquiler-viviendas/huesca-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/lleida-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/alt-penedes/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/anoia/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/bages/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/baix-llobregat-nord/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/baix-llobregat-sud/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona-barcelona/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/barcelones/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/bergueda/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/garraf/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/maresme/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/moianes/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/osona/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/valles-occidental/",
    "https://www.idealista.com/en/alquiler-viviendas/barcelona/valles-oriental/",
    "https://www.idealista.com/en/alquiler-viviendas/girona-provincia/",
    "https://www.idealista.com/en/alquiler-viviendas/formentera-balears-illes/",
    "https://www.idealista.com/en/alquiler-viviendas/balears-illes/ibiza/",
    "https://www.idealista.com/en/alquiler-viviendas/balears-illes/mallorca/",
    "https://www.idealista.com/en/alquiler-viviendas/balears-illes/menorca/",
    "https://www.idealista.com/en/alquiler-viviendas/santa-cruz-de-tenerife/el-hierro/",
    "https://www.idealista.com/en/alquiler-viviendas/santa-cruz-de-tenerife/la-gomera/",
    "https://www.idealista.com/en/alquiler-viviendas/santa-cruz-de-tenerife/la-palma/",
    "https://www.idealista.com/en/alquiler-viviendas/santa-cruz-de-tenerife/tenerife/"
"""
