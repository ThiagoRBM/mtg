# https://docs.magicthegathering.io/
import requests
import settings
import re
import pandas as pd
import time


def get_allcards():
    """Funcao que pega todas as cartas disponiveis no banco de dados,
    levando em conta o numero de cartas e o máximo mostrado por vez
    (em 18/02/2023). Pega as informações de todas em loop.
    """
    response = requests.get(settings.URL_BASE)
    pgs = []
    if response.status_code == 200:
        max_page = re.search(
            "(?<=page\\=).*", response.links["last"]["url"]
        ).group(0)

        for pag in range(1, int(max_page) + 1):
            data = requests.get(
                f"http://api.magicthegathering.io/v1/cards?page={pag}"
            )
            data = data.json()
            pgs.append(data)
            print(f"cartas página {pag}")

            if pag % 10 == 0:
                time.sleep(1)
                print(f"\nesperando 1 segundo\n")

        return pgs

    else:
        print("Request failed with status code", response.status_code)


def cards_to_dataframe(list_dict):
    """Funcao que pega as cartas obtidas na função 'get_all_cards' e tranforma
    em um pd.DataFrame.
    """
    busca_final = []
    for pag in range(0, len(list_dict)):
        pg = list_dict[pag]["cards"]
        cartas = []

        for linha in range(0, len(pg)):
            # breakpoint()
            carta = pd.DataFrame.from_dict(pg[linha], orient="index")
            cartas.append(carta.T)

        df_pag = pd.concat(cartas)
        busca_final.append(df_pag)

    tabela = pd.concat(busca_final)

    return tabela


def save_to_csv(cards_pd, nome):
    cards_pd.to_csv(f"{settings.PATH}/{nome}.csv", index=False)
    print(f"arquivo salvo em: \n{settings.PATH}")


def get_set_date():
    """Funcao que recebe uma lista de nomes de Sets e busca a data de publicação
    deles
    """
    response = requests.get(settings.URLS_SETS)
    breakpoint()
    max_page = re.search(
        "(?<=page\\=).*", response.links["last"]["url"]
    ).group(0)
    pgs = []

    for pg in range(1, int(max_page) + 1):
        response = requests.get(
            f"http://api.magicthegathering.io/v1/sets?pageSize=500&page={pg}"
        )

        sets = response.json()
        ls_sets = []

        for set in sets["sets"]:

            dict_ = {
                "code": set["code"],
                "name": set["name"],
                "releaseDate": set["releaseDate"],
                "block": set.get("block", "NA"),
            }

            ls_sets.append(dict_)

        df = pd.DataFrame.from_dict(ls_sets, orient="columns")
        pgs.append(df)

    df_final = pd.concat(pgs)

    return df_final
