#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import xml.etree.ElementTree as ET
import os
import re
import argparse
import sys
from datetime import datetime

def validate_base_version_format(version: str) -> bool:
    """
    Validates if the base version follows the format x.y.z
    """
    pattern = r"\d+\.\d+\.\d+"
    return re.fullmatch(pattern, version) is not None

def generate_full_version(base_version: str) -> str:
    """
    Generates the full version by appending YYMM to the base version
    where YYMM represents current year (2 digits) + month (2 digits)
    
    Args:
        base_version: Base version in format x.y.z
        
    Returns:
        str: Full version in format x.y.z.YYMM
    """
    now = datetime.now()
    yy = str(now.year)[-2:]  # Last 2 digits of year
    mm = f"{now.month:02d}"  # Month with leading zero if needed
    yymm = yy + mm
    
    return f"{base_version}.{yymm}"

def generate_copyright() -> str:
    """
    Generates the copyright string with current year
    
    Returns:
        str: Copyright string in format "© Copyleft 2018-YYYY"
    """
    current_year = datetime.now().year
    return f"© Copyleft 2018-{current_year}"

def update_product_version(lpi_path: str, base_version: str) -> bool:
    """
    Updates the ProductVersion and LegalCopyright in a Lazarus Project Information (.lpi) file
    
    Args:
        lpi_path: Path to the .lpi file
        base_version: Base version string in format x.y.z (YYMM will be auto-generated)
        
    Returns:
        bool: True if successful, False otherwise
    """
    if not validate_base_version_format(base_version):
        print(f"Error: Invalid base version format: {base_version} (expected x.y.z)")
        return False

    # Generate the full version with current YYMM
    full_version = generate_full_version(base_version)
    print(f"Generated full version: {full_version}")
    
    # Generate the copyright string
    copyright_text = generate_copyright()
    print(f"Generated copyright: {copyright_text}")

    full_path = os.path.abspath(lpi_path)

    if not os.path.exists(full_path):
        print(f"Error: File not found: {full_path}")
        return False

    if not full_path.lower().endswith('.lpi'):
        print(f"Error: File is not a .lpi file: {full_path}")
        return False

    try:
        tree = ET.parse(full_path)
        root = tree.getroot()

        # Look for the specific path: /CONFIG/ProjectOptions/VersionInfo/StringTable
        # In .lpi files, StringTable is an attribute container, not a separate element
        string_table = root.find(".//ProjectOptions/VersionInfo/StringTable")
        
        if string_table is not None:
            # Update ProductVersion
            string_table.set("ProductVersion", full_version)
            print(f"Success: ProductVersion updated to {full_version}")
            
            # Update LegalCopyright
            string_table.set("LegalCopyright", copyright_text)
            print(f"Success: LegalCopyright updated to {copyright_text}")
            
            # Write the changes to file
            tree.write(full_path, encoding="utf-8", xml_declaration=True, short_empty_elements=True)
            print(f"File successfully updated: {full_path}")
            return True
        else:
            print(f"Error: StringTable not found in path ProjectOptions/VersionInfo/StringTable in {full_path}")
            return False
            
    except ET.ParseError as e:
        print(f"Error: Failed to parse XML file {full_path}: {e}")
        return False
    except Exception as e:
        print(f"Error: Unexpected error while processing {full_path}: {e}")
        return False

def main():
    """
    Main function to handle command line arguments and execute version update
    """
    parser = argparse.ArgumentParser(
        description="Update ProductVersion and LegalCopyright in Lazarus Project Information (.lpi) files",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python version.py project.lpi 1.0.0
  python version.py /path/to/project.lpi 2.1.3
  
The script will automatically:
- Append .YYMM to version based on current date (e.g., 1.0.0.2508)
- Set LegalCopyright to "© Copyleft 2018-YYYY" where YYYY is current year
        """
    )
    
    parser.add_argument(
        "lpi_file",
        help="Path to the Lazarus Project Information (.lpi) file"
    )
    
    parser.add_argument(
        "version",
        help="Base version number in format x.y.z (YYMM will be auto-generated based on current date)"
    )
    
    args = parser.parse_args()
    
    # Update the version and copyright
    success = update_product_version(args.lpi_file, args.version)
    
    # Exit with appropriate code
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()