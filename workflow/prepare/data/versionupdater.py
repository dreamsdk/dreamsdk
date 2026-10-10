#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import xml.etree.ElementTree as ET
import os
import re
import argparse
import sys
from datetime import datetime
from xml.sax.saxutils import escape

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

def set_tag_attribute(tag: str, name: str, value: str) -> str:
    """
    Sets an attribute value in a single XML tag (given as text), keeping the
    rest of the tag untouched. The attribute is added if missing.

    Args:
        tag: XML tag text, e.g. '<StringTable ProductVersion="1.0"/>'
        name: Attribute name
        value: New attribute value (not escaped)

    Returns:
        str: The updated tag text
    """
    escaped_value = escape(value, {'"': "&quot;"})
    pattern = re.compile(r'(\s' + re.escape(name) + r'\s*=\s*)(["\'])(.*?)\2', re.DOTALL)
    if pattern.search(tag):
        return pattern.sub(lambda m: f'{m.group(1)}{m.group(2)}{escaped_value}{m.group(2)}', tag, count=1)
    # Insert before the end of the tag ("/>" or ">")
    end = re.search(r'\s*/?>$', tag)
    return f'{tag[:end.start()]} {name}="{escaped_value}"{tag[end.start():]}'

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
        
        if string_table is None:
            print(f"Error: StringTable not found in path ProjectOptions/VersionInfo/StringTable in {full_path}")
            return False

        # The XML tree is only used to validate the file: rewriting it would
        # reformat the whole file (quotes, spaces before "/>"...), so only the
        # StringTable tag is changed in the original text. The file is read as
        # bytes to keep the line endings and BOM (if any) as they are.
        with open(full_path, 'rb') as f:
            content = f.read().decode('utf-8')

        version_info = re.search(r'<VersionInfo\b[^>]*>.*?</VersionInfo>', content, re.DOTALL)
        string_table_tag = version_info and re.search(r'<StringTable\b[^>]*>', version_info.group(0))
        if not string_table_tag:
            print(f"Error: StringTable tag not found in VersionInfo in {full_path}")
            return False

        old_tag = string_table_tag.group(0)
        new_tag = set_tag_attribute(old_tag, "ProductVersion", full_version)
        print(f"Success: ProductVersion updated to {full_version}")
        new_tag = set_tag_attribute(new_tag, "LegalCopyright", copyright_text)
        print(f"Success: LegalCopyright updated to {copyright_text}")

        if new_tag == old_tag:
            print(f"File already up to date: {full_path}")
            return True

        start = version_info.start() + string_table_tag.start()
        content = content[:start] + new_tag + content[start + len(old_tag):]

        # Write the changes to file
        with open(full_path, 'wb') as f:
            f.write(content.encode('utf-8'))
        print(f"File successfully updated: {full_path}")
        return True
            
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