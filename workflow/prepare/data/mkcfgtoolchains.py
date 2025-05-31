#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os
import configparser
from datetime import datetime

def normalize_profile(profile):
    """
    Normalize profile name: first letter capitalized, rest lowercase.
    STABLE/Stable/stable -> Stable
    950WINXP/950winxp -> 950winxp (special case handling)
    
    Args:
        profile (str): Profile name to normalize
        
    Returns:
        str: Normalized profile name
    """
    # Special case for version-like profiles (e.g., 950WINXP)
    if profile[0].isdigit():
        return profile.lower()
    
    # Standard case: capitalize first letter, rest lowercase
    return profile.capitalize()

def get_profile_sort_key(profile):
    """
    Get sort key for a profile to ensure STABLE comes first.
    
    Args:
        profile (dict): Profile dictionary with 'profile' key
        
    Returns:
        tuple: Sort key (priority, profile_name)
    """
    profile_name = profile['profile'].lower()
    
    # Give STABLE highest priority (lowest sort value)
    if profile_name == 'stable':
        return (0, profile_name)
    
    # Sort the rest alphabetically
    return (1, profile_name)

def parse_toolchains_config(config_path):
    """
    Parse the toolchains configuration file.
    
    Args:
        config_path (str): Path to the configuration INI file
        
    Returns:
        tuple: (toolchains32_data, toolchains64_data)
    """
    config = configparser.ConfigParser()
    config.read(config_path)
    
    def parse_toolchains_data(bitness):
        """Helper function to parse toolchain data for a specific bitness"""
        section_key = f'Toolchains{bitness}'
        versions_key = f'TOOLCHAINS{bitness}_VERSIONS'
        
        toolchains_data = []
        versions = config.get('Toolchains', versions_key).split(';')
        
        for version in versions:
            name_key = f'{section_key}_VERSION_NAME_{version}'
            desc_key = f'{section_key}_VERSION_DESC_{version}'
            
            if config.has_option(section_key, name_key) and config.has_option(section_key, desc_key):
                toolchain_name = config.get(section_key, name_key)
                toolchain_desc = config.get(section_key, desc_key)
                profile_key = version.lower() if version.lower() != 'stable' else 'stable'
                
                armeabi_key = f'{section_key}_VERSION_PACKAGE_ARMEABI_{version}'
                shelf_key = f'{section_key}_VERSION_PACKAGE_SHELF_{version}'
                
                armeabi_package = config.get(section_key, armeabi_key) if config.has_option(section_key, armeabi_key) else ""
                shelf_package = config.get(section_key, shelf_key) if config.has_option(section_key, shelf_key) else ""
                
                toolchains_data.append({
                    'profile': profile_key,
                    'name': toolchain_name,
                    'description': toolchain_desc,
                    'armeabi': armeabi_package,
                    'shelf': shelf_package
                })
        
        # Sort data with STABLE first, then alphabetically
        return sorted(toolchains_data, key=get_profile_sort_key)
    
    toolchains32_data = parse_toolchains_data('32')
    toolchains64_data = parse_toolchains_data('64')
    
    return (toolchains32_data, toolchains64_data)

def generate_source_directory_definitions(toolchains_data, bitness):
    """
    Generate source directory definitions for toolchains.
    
    Args:
        toolchains_data (list): List of toolchain data dictionaries
        bitness (str): '32' or '64' for architecture bitness
        
    Returns:
        str: Source directory definitions as text
    """
    content = ""
    
    for toolchain in toolchains_data:
        profile = toolchain['profile']
        profile_normalized = normalize_profile(profile)
        content += f'#define SourceDirectoryToolchain{bitness}_{profile_normalized} SourceDirectoryBase + "\\toolchain-{profile}{"-x64" if bitness == "64" else ""}"\n'
    
    return content

def generate_custom_messages_section(toolchains_data, bitness):
    """
    Generate CustomMessages section entries for a specific bitness.
    
    Args:
        toolchains_data (list): List of toolchain data dictionaries
        bitness (str): '32' or '64' for architecture bitness
        
    Returns:
        str: CustomMessages section entries
    """
    content = ""
    
    for toolchain in toolchains_data:
        profile = toolchain['profile']
        profile_normalized = normalize_profile(profile)
        name = toolchain['name']
        desc = toolchain['description']
        
        content += f'ToolchainName{bitness}_{profile_normalized}={name}\n'
        content += f'ToolchainDesc{bitness}_{profile_normalized}={desc}\n'
    
    return content

def generate_initialization_code_section(toolchains_data, bitness):
    """
    Generate initialization code section for a specific bitness.
    
    Args:
        toolchains_data (list): List of toolchain data dictionaries
        bitness (str): '32' or '64' for architecture bitness
        
    Returns:
        str: Initialization code section
    """
    arch_desc = "x86" if bitness == "32" else "x64"
    content = f"  // {bitness}-bit ({arch_desc})\n"
    content += f"  SetArrayLength(Toolchain{bitness}Packages, {{#Toolchain{bitness}Count}});\n\n"
    
    for i, toolchain in enumerate(toolchains_data):
        profile = toolchain['profile']
        profile_normalized = normalize_profile(profile)
        
        content += f"  // {toolchain['name']}\n"
        content += f"  Toolchain{bitness}Packages[{i}].Name := ExpandConstant('{{cm:ToolchainName{bitness}_{profile_normalized}}}');\n"
        content += f"  Toolchain{bitness}Packages[{i}].Description := ExpandConstant('{{cm:ToolchainDesc{bitness}_{profile_normalized}}}');\n"
        
        # Add a newline between toolchains but not after the last one
        if i < len(toolchains_data) - 1:
            content += "  \n"
    
    return content

def generate_files_section_entries(toolchains_data, bitness):
    """
    Generate [Files] section entries for a specific bitness.
    
    Args:
        toolchains_data (list): List of toolchain data dictionaries
        bitness (str): '32' or '64' for architecture bitness
        
    Returns:
        str: Files section entries
    """
    arch_desc = "x86" if bitness == "32" else "x64"
    content = f"; {bitness}-bit ({arch_desc})\n"
    
    for toolchain in toolchains_data:
        profile = toolchain['profile']
        profile_normalized = normalize_profile(profile)
        
        content += f'Source: "{{#SourceDirectoryToolchain{bitness}_{profile_normalized}}}\\*"; '
        content += 'DestDir: "{code:GetApplicationToolchainBasePath}"; '
        content += 'Flags: ignoreversion recursesubdirs createallsubdirs; '
        content += f'Components: "main\\toolchains\\{bitness}\\{profile}"\n'
    
    return content

def generate_components_section_entries(toolchains_data, bitness):
    """
    Generate [Components] section entries for a specific bitness.
    
    Args:
        toolchains_data (list): List of toolchain data dictionaries
        bitness (str): '32' or '64' for architecture bitness
        
    Returns:
        str: Components section entries
    """
    arch_desc = "x86" if bitness == "32" else "x64"
    content = f"; {bitness}-bit ({arch_desc})\n"
    content += f'Name: "main\\toolchains\\{bitness}"; Description: "{{cm:ComponentToolchain{bitness}}}"; Flags: fixed\n'
    
    for toolchain in toolchains_data:
        profile = toolchain['profile']
        profile_normalized = normalize_profile(profile)
        
        content += f'Name: "main\\toolchains\\{bitness}\\{profile}"; '
        content += f'Description: "{{cm:ToolchainName{bitness}_{profile_normalized}}}"; '
        content += 'Flags: exclusive fixed\n'
    
    return content

def generate_toolchains_config(config_path, output_path):
    """
    Generate toolchains configuration file based on versions and data found in the specified INI file.
    
    Args:
        config_path (str): Path to the configuration INI file
        output_path (str): Path to the output directory
        
    Returns:
        bool: True if successful, False otherwise
    """
    try:
        # Parse the configuration
        toolchains32_data, toolchains64_data = parse_toolchains_config(config_path)
        
        # Get current date and time
        current_datetime = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        
        # Header
        content = """; =============================================================================
;  ____                         _____  ____   _____
; |    .  ___  ___  ___  _____ |   __||    . |  |  |
; |  |  ||  _|| -_|| .'||     ||__   ||  |  ||    -|
; |____/ |_|  |___||__,||_|_|_||_____||____/ |__|__|
;
; =============================================================================
; DreamSDK Setup - Toolchains Configuration
; =============================================================================
;
; THIS FILE HAS BEEN GENERATED ON """ + current_datetime + """.
; PLEASE DON'T UPDATE IT.

"""
        
        # Process each bitness
        for bitness, toolchains_data in [("32", toolchains32_data), ("64", toolchains64_data)]:
            # Add bitness count
            arch_desc = "x86" if bitness == "32" else "x64"
            content += f"; {bitness}-bit ({arch_desc})\n"
            content += f"#define Toolchain{bitness}Count {len(toolchains_data)}\n\n"
            
            # Add source directory definitions
            content += generate_source_directory_definitions(toolchains_data, bitness)
            
            # Add newline between sections
            if bitness == "32" and toolchains64_data:
                content += "\n"
        
        # Add custom messages
        content += "\n[CustomMessages]\n"
        content += generate_custom_messages_section(toolchains32_data, "32")
        content += generate_custom_messages_section(toolchains64_data, "64")
        
        # Add initialization code
        content += "\n[Code]\n"
        content += "procedure InitializeArrayToolchain();\n"
        content += "begin\n"
        content += generate_initialization_code_section(toolchains32_data, "32")
        
        if toolchains64_data:
            content += "\n"
            content += generate_initialization_code_section(toolchains64_data, "64")
        
        content += "end;\n"
        
        # Add files section
        content += "\n[Files]\n"
        content += generate_files_section_entries(toolchains32_data, "32")
        
        if toolchains64_data:
            content += "\n"
            content += generate_files_section_entries(toolchains64_data, "64")
        
        # Add components section
        content += "\n[Components]\n"
        content += generate_components_section_entries(toolchains32_data, "32")
        
        if toolchains64_data:
            content += "\n"
            content += generate_components_section_entries(toolchains64_data, "64")
        
        # Write to output file
        full_output_path = os.path.join(output_path, 'toolchains.context.iss')
        with open(full_output_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"Toolchains configuration file successfully generated at: {full_output_path}")
        return True
        
    except Exception as e:
        print(f"Error generating toolchains configuration: {str(e)}")
        return False

def main():
    """
    Main function that processes command line arguments and generates the file.
    """
    if len(sys.argv) != 3:
        print("Usage: python mkcfgtoolchains.py <output_path> <config_ini_path>")
        print("Example: python mkcfgtoolchains.py /path/to/output packages.default.ini")
        sys.exit(1)

    output_path = sys.argv[1]
    config_path = sys.argv[2]
    
    if not os.path.exists(config_path):
        print(f"Error: Configuration file {config_path} does not exist")
        sys.exit(1)
    
    if not os.path.exists(output_path):
        try:
            os.makedirs(output_path)
        except Exception as e:
            print(f"Error creating output directory: {str(e)}")
            sys.exit(1)
    
    # Generate the toolchains configuration
    success = generate_toolchains_config(config_path, output_path)
    
    if not success:
        sys.exit(1)

if __name__ == "__main__":
    main()
