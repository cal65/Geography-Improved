from geopy.geocoders import Nominatim
import json
import pandas as pd
import numpy as np
from datetime import timedelta
import h3
import logging

logging.basicConfig()
ch = logging.StreamHandler()
ch.setLevel(logging.INFO)
# add the handler to the root logger
logger = logging.getLogger(__name__)
logger.addHandler(ch)


def convert_location_json(json_path):
    """
    Reads in a Json object that is the extract of a Google Location History export
    Returns pandas dataframe
    """
    with open(json_path, "r") as f:
        data = json.loads(f.read())
    # Flatten data
    df = pd.json_normalize(data, record_path=["locations"])
    return df


def misc_clean(df):
    df["ds"] = pd.to_datetime(df["timestampMs"], unit="ms")
    df["lat"] = df["latitudeE7"] / 10000000
    df["lon"] = df["longitudeE7"] / 10000000
    df["date"] = df["ds"].dt.date
    df["year"] = df["ds"].dt.year
    return df

def identify_missing_dates(df, date_col="date"):
    date_start = df[date_col].min()
    date_end = df[date_col].max()
    date_range = pd.date_range(start=date_start, end=date_end, freq='D').date
    date_range = pd.to_datetime(date_range)
    dates_missing = set(date_range).difference(set(df[date_col]))
    dates_missing = list(dates_missing)
    dates_missing.sort()
    return dates_missing

#geolocator = Nominatim(user_agent="geoapiExercises")
geolocator = Nominatim(user_agent="google")

def reverse_geolocate(row, nom_obj, lat="lat", lon="lon"):
    location_obj = nom_obj.reverse(str(row["lat"]) + "," + str(row["lon"]), language='en')
    return pd.DataFrame([location_obj.raw["address"]])


def fill_missing_city(row, city_col="city", levels=["town", "village", "county"]):
    if pd.isnull(row[city_col]):
        for level in levels:
            city = row.get(level)
            if pd.notnull(city):
                return city
        return None
    else:
        return row[city_col]


def get_last(df):
    return df.tail(1)

def separate_address(daily_df, coordinate_cutoff = 0.1, date_col = "date"):
    coords_stds = (
        pd.pivot_table(daily_df, values=["lat", "lon"], columns=[date_col], aggfunc=np.std)
        .transpose()
        .reset_index()
    )
    # different logic for days of high movement and days of low movement
    movement_high = coords_stds[coords_stds["lat"] > 0.1]
    movement_low = coords_stds[coords_stds["lat"] <= 0.1]
    def _return_location_pivot(df, movement_df, aggfunc):
        return (
        pd.pivot_table(
            df[df[date_col].isin(movement_df[date_col])],
            values=["lat", "lon"],
            columns=[date_col],
            aggfunc=aggfunc,
        )
        .transpose()
        .reset_index()
        )
    # for days with low movement, take the median lat/lon pair
    locations_low = _return_location_pivot(daily_df, movement_low, aggfunc='median')
    # for days with high movement, take the last lat/lon pair
    locations_high = _return_location_pivot(daily_df, movement_low, aggfunc=get_last)
    return locations_low, locations_high

def combine_addresses(daily_df, coordinate_cutoff = 0.1, date_col = "date"):
    locations_low, locations_high = separate_address(daily_df, coordinate_cutoff=coordinate_cutoff, date_col=date_col)
    locations = pd.concat([locations_low, locations_high]).sort_values(date_col)
    return(locations)

def create_h3_df(df, res):
    df['h3'] = df.apply(lambda x: h3.geo_to_h3(x['lat'], x['lon'], resolution=res), axis=1)
    h3_df = df.groupby('h3').median()[['lat', 'lon']]
    h3_df.reset_index(inplace=True)
    return h3_df

def get_addresses_from_h3(h3_df):
    logger.info('applying reverse geolocate for rows: ' + str(h3_df.shape[0]))
    addresses = h3_df.apply(reverse_geolocate, nom_obj=geolocator, axis=1)
    addresses_df = pd.concat(addresses.tolist())
    addresses_df.reset_index(inplace=True, drop=True)
    h3_addresses = pd.concat(
        [
            h3_df,
            addresses_df[["city", "county", "state", "country", "country_code"]],
        ],
        axis=1,
        ignore_index=False,
    )
    return h3_addresses
#     locations_df = pd.merge(locations, h3_addresses.drop(columns=['lat', 'lon']), on ='h3')

#     addresses_high = locations_high.apply(reverse_geolocate, nom_obj=geolocator, axis=1)
#     addresses_high_df = pd.concat(addresses_high.tolist())
#     addresses_high_df.reset_index(inplace=True, drop=True)
#     addresses_high_df.loc[
#         pd.isnull(addresses_high_df["city"]), "city"
#     ] = addresses_high_df[pd.isnull(addresses_high_df["city"])].apply(
#         fill_missing_city, axis=1
#     )
#     addresses_low_df.loc[
#         pd.isnull(addresses_low_df["city"]), "city"
#     ] = addresses_low_df[pd.isnull(addresses_low_df["city"])].apply(
#         fill_missing_city, axis=1
#     )
#     locations_high_df = pd.concat(
#         [
#             locations_high,
#             addresses_high_df[["city", "county", "state", "country", "country_code"]],
#         ],
#         axis=1,
#         ignore_index=False,
#     )
#     locations_df = pd.concat([locations_low_df, locations_high_df]).sort_values("date")
#     locations_df.reset_index(drop=True, inplace=True)

#     return locations_df

def fill_missing_dates(df, date_col='date'):
    date_start = df[date_col].min()
    date_end = df[date_col].max()
    date_range = pd.date_range(start=date_start, end=date_end, freq='D').date
    date_df = pd.DataFrame({date_col: date_range})
    #date_df[date_col] = date_df[date_col].astype(str)
    combined_df = pd.merge(date_df, df, on=date_col)
    return combined_df



def simplify_row(location_row1, location_row2):
    """
    Return a new dataframe row if the city and state are different
    Else
    """
    city1, state1, date1 = location_row1[["city", "state", "date"]]
    city2, state2, date2 = location_row2[["city", "state", "date"]]
    country = location_row1["country"]  # because we aren't comparing this, just returning it
    h3 = location_row1["h3"]
    date1 = pd.to_datetime(date1)
    date2 = pd.to_datetime(date2)
    if (city1 != city2) or (state1 != state2):
        return_row = pd.DataFrame(
            {
                "city": [city1],
                "state": [state1],
                "country": [country],
                "date_start": [date1],
                "date_end": [date2],
                "h3": [h3],
            }
        )
        return return_row
    else:
        return
    

def simplify_df(df):
    row1 = df.iloc[0]
    simplified_rows = []
    for row in df[1:].iterrows():
        return_row = simplify_row(
            row1, row[1]
        )  # [1] because iterrows returns a tuple of (index, row)
        if return_row is not None:
            simplified_rows.append(return_row)
            row1 = row[1]
    simplified_df = pd.concat(simplified_rows).reset_index(drop=True)
    return simplified_df


def apply_pipeline(json_path):
    location_df = convert_location_json(json_path)
    location_df = misc_clean(location_df)
    df["city"] = df.apply(fill_missing_city, axis=1)
    ms_df = simplify_df(df)