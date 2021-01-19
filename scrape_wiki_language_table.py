from bs4 import BeautifulSoup
import requests
import pandas as pd

url = (
    "https://en.wikipedia.org/wiki/List_of_official_languages_by_country_and_territory"
)
page = requests.get(url)
soup = BeautifulSoup(page.content, "html.parser")

language_table = soup.find("table", {"class": "wikitable"})

countries = []
languages = []
regionals = []
for i, row in enumerate(language_table.findAll("tr")):
    cells = row.findAll("td")
    if len(cells) > 0:  # ignore some table headers with no data
        countries.append(cells[0].find("a").get("title"))
        try:
            cell1 = row.findAll("td")[1]
            language_list = list(filter(None, cell1.findAll("li").text.split("\n")))
            languages.append(language_list)
        except:
            language_list = list(filter(None, cell1.text.split("\n")))
            languages.append(language_list)
        try:
            rs = cells[2].findAll("a")
            r = []
            for regional_lang in rs:
                r.append(regional_lang.get("title"))
            regionals.append(r)
        except:
            regionals.append(None)

countries = [country.replace("Languages of ", "") for country in countries]

wiki_lang_df = pd.DataFrame(
    {"Country": countries, "Languages": languages, "Regionals": regionals}
)
wiki_lang_df = wiki_lang_df.explode("Languages", ignore_index=True)
wiki_lang_df["Languages"] = wiki_lang_df["Languages"].str.replace(
    "\\(.*\\)", "", regex=True
)
wiki_lang_df["Languages"] = wiki_lang_df["Languages"].str.replace(
    "\\[.*\\]", "", regex=True
)
wiki_lang_df["Languages"] = wiki_lang_df["Languages"].str.replace(" language", "")
wiki_lang_df["Languages"] = wiki_lang_df["Languages"].str.strip()

wiki_lang_df = wiki_lang_df[pd.notnull(wiki_lang_df["Languages"])]
## manually add
added_df = pd.DataFrame(
    {
        "Country": ["Hong Kong", "Macau", "India"],
        "Languages": ["Cantonese", "Cantonese", "Hindi"],
        "Regionals": ["", "", ""],
    }
)
wiki_lang_df = pd.concat([wiki_lang_df, added_df])

wiki_lang_df.to_csv("wikipedia_language_table.csv", index=False)
