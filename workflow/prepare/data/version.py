import xml.etree.ElementTree as ET
import os
import re
from configparser import ConfigParser

def update_product_version(xml_paths: list[str], new_version: str) -> None:
    if not re.fullmatch(r"\d+\.\d+\.\d+", new_version):
        raise ValueError(f"Format de version invalide : {new_version} (attendu x.y.z)")

    for xml_path in xml_paths:
        full_path = os.path.abspath(xml_path)

        if not os.path.exists(full_path):
            print(f"Fichier introuvable : {full_path}")
            continue

        tree = ET.parse(full_path)
        root = tree.getroot()

        version_info = root.find(".//ProjectOptions/VersionInfo/StringTable")
        if version_info is not None:
            version_info.set("ProductVersion", new_version)
            tree.write(full_path, encoding="utf-8", xml_declaration=True)
            print(f"ProductVersion mis à jour vers {new_version} dans {full_path}")
        else:
            print(f"<StringTable> introuvable dans {full_path}")

def load_config(config_path: str) -> tuple[list[str], str]:
    parser = ConfigParser()
    parser.read(config_path, encoding="utf-8")

    xml_file_paths = [p.strip() for p in parser.get("Settings", "xml_file_paths").split(",")]
    new_version_value = parser.get("Settings", "new_version_value")

    return xml_file_paths, new_version_value

if __name__ == "__main__":
    config_file = "config.ini"  # Modifie avec le chemin réel du fichier INI
    xml_file_paths, new_version_value = load_config(config_file)
    update_product_version(xml_file_paths, new_version_value)
