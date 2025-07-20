#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os
import re
import subprocess

def get_7z_uncompressed_size(seven_zip_path, archive_path):
    """
    Get the uncompressed size of a 7z archive using 7z command line tool.
    
    Args:
        seven_zip_path (str): Path to 7z.exe
        archive_path (str): Path to the 7z archive
        
    Returns:
        int: Uncompressed size in bytes, or 0 if error
    """
    try:
        # Check if archive exists
        if not os.path.exists(archive_path):
            return 0
        
        # Run 7z list command
        result = subprocess.run([seven_zip_path, 'l', archive_path], 
                               capture_output=True, text=True, encoding='utf-8')
        
        if result.returncode != 0:
            return 0
        
        # Parse the output to find the total uncompressed size
        # Look for the last line with the format: "date time size compressed_size files folders"
        lines = result.stdout.strip().split('\n')        
        for line in reversed(lines):
            # Look for the summary line that contains file/folder counts
            if 'files' in line and 'folders' in line:
                # Extract numbers from the line
                numbers = re.findall(r'\d+', line)                
                if len(numbers) >= 2:
                    # The 6th number should be the uncompressed size
                    return int(numbers[6])
        
        return 0
        
    except Exception as e:
        print(f"Warning: Failed to get size for {archive_path}: {str(e)}")
        return 0

def calculate_package_size(seven_zip_path, packages_path, pkg_version):
    """
    Calculate the size of a package by reading its 7z archive.
    
    Args:
        seven_zip_path (str): Path to 7z.exe
        packages_path (str): Path to the directory containing 7z packages
        pkg_version (str): version
        
    Returns:
        int: Package size in bytes, or 0 if not found/error
    """    
    # Construct the full path to the 7z file
    if not pkg_version:
        pkg_name = "no-python"
    else:
        pkg_name = f"python-{pkg_version}"
    archive_path = os.path.join(packages_path, f"sh-elf-gdb-{pkg_name}-bin.7z")
    
    return get_7z_uncompressed_size(seven_zip_path, archive_path)

def scan_packages(base_path):
    """
    Scan the directory to find GDB packages.
    
    Args:
        base_path (str): Path to the directory containing GDB packages
        
    Returns:
        list: List of tuples (package_name, python_version)
    """
    packages = []
    
    # Ensure the base path exists
    if not os.path.exists(base_path):
        print(f"Warning: Path {base_path} does not exist")
        return packages
    
    # Pattern to match directories like "sh-elf-gdb-no-python" or "sh-elf-gdb-python-X.Y"
    pattern = r"sh-elf-gdb-(?:python-([0-9]+\.[0-9]+)|no-python)-bin\.7z"
    
    # Look for subdirectories in the base path
    for item in os.listdir(base_path):
        full_path = os.path.join(base_path, item)
        
        # Check if it's a directory
#        if os.path.isdir(full_path):
        if item.endswith('.7z'):
            # Check if the directory name matches our pattern
            if "sh-elf-gdb" in item:
                match = re.search(pattern, item)
                if match:
                    # If it has a Python version
                    if match.group(1):
                        python_version = match.group(1)
                        package_name = f"Python {python_version}"
                        packages.append((package_name, python_version))
                    # If it's the no-python variant
                    else:
                        packages.append(("None", ""))
    
    # Sort packages: None first, then by Python version numerically
    def sort_key(pkg):
        if pkg[0] == "None":
            return (-1, -1)  # Place None first
        
        # Split version into parts and convert to integers for proper numerical sorting
        version_parts = pkg[1].split(".")
        major = int(version_parts[0])
        minor = int(version_parts[1])
        return (major, minor)
    
    return sorted(packages, key=sort_key)

def get_formatted_python_version(version):
    """
    Format Python version for use in identifiers (e.g., 3.10 -> 310)
    
    Args:
        version (str): Python version (e.g., "3.10")
        
    Returns:
        str: Formatted version (e.g., "310")
    """
    if not version or version == "nopython":
        return None
    return version.replace(".", "")

def generate_custom_messages(packages_32bit, packages_64bit):
    """
    Generate CustomMessages section based on available packages.
    
    Args:
        packages_32bit (list): List of packages for 32-bit
        packages_64bit (list): List of packages for 64-bit
        
    Returns:
        str: CustomMessages section content
    """
    # Combine all packages from both architectures
    all_packages = []
    for pkg_name, pkg_version in packages_32bit + packages_64bit:
        if (pkg_name, pkg_version) not in all_packages:
            all_packages.append((pkg_name, pkg_version))
    
    # Sort the combined packages
    def sort_key(pkg):
        if pkg[0] == "None":
            return (-1, -1)  # Place None first
        
        # Split version into parts and convert to integers for proper numerical sorting
        if pkg[1]:
            version_parts = pkg[1].split(".")
            major = int(version_parts[0])
            minor = int(version_parts[1])
            return (major, minor)
        return (0, 0)
    
    all_packages = sorted(all_packages, key=sort_key)
    
    # Generate CustomMessages section
    content = "[CustomMessages]\n"
    
    # Add None package
    content += "GdbPython_None=Don't enable Python extensions for GNU Debugger (GDB)\n"
    
    # Add Python packages
    for pkg_name, pkg_version in all_packages:
        if pkg_version:  # Skip None package as we already added it
            formatted_version = get_formatted_python_version(pkg_version)
            content += f"GdbPython_{formatted_version}=Python {pkg_version}\n"
    
    return content

def generate_source_directories(packages, arch):
    """
    Generate the source directory definitions for the specified architecture.
    
    Args:
        packages (list): List of packages (name, version) tuples
        arch (str): Architecture type ('32' or '64')
        
    Returns:
        str: Source directory definitions as text
    """
    content = ""
    
    for pkg_name, pkg_version in packages:
        if pkg_version:
            formatted_version = get_formatted_python_version(pkg_version)
            content += f'#define SourceDirectoryGdb{arch}Python{formatted_version} SourceDirectoryBase + "\\sh-elf-gdb{"-x64" if arch == "64" else ""}\\sh-elf-gdb-python-{pkg_version}"\n'
        else:
            content += f'#define SourceDirectoryGdb{arch} SourceDirectoryBase + "\\sh-elf-gdb{"-x64" if arch == "64" else ""}\\sh-elf-gdb-no-python"\n'
    
    return content

def generate_package_initializations(packages, arch):
    """
    Generate the package initialization code for the specified architecture.
    
    Args:
        packages (list): List of packages (name, version) tuples
        arch (str): Architecture type ('32' or '64')
        
    Returns:
        str: Package initialization code as text
    """
    arch_desc = "x86" if arch == "32" else "x64"
    content = f"  // {arch}-bit ({arch_desc})\n"
    content += f"  InitializeGdb{arch}Packages({{#Gdb{arch}Count}}, '{{#Gdb{arch}Version}}');\n\n"
    
    for i, (pkg_name, pkg_version) in enumerate(packages):
        formatted_version = get_formatted_python_version(pkg_version)
        content += f"  // {pkg_name}\n"
        content += f"  Gdb{arch}Packages[{i}].Name := ExpandConstant('{{cm:GdbPython_{formatted_version}}}');\n"
        content += f"  Gdb{arch}Packages[{i}].Version := '{pkg_version}';\n"
        # Add newline between packages but not after the last one
        if i < len(packages) - 1:
            content += "\n"
    
    return content

def generate_file_entries(packages, arch):
    """
    Generate the [Files] section entries for the specified architecture.
    
    Args:
        packages (list): List of packages (name, version) tuples
        arch (str): Architecture type ('32' or '64')
        
    Returns:
        str: File entries as text
    """
    content = f"; {arch}-bit ({'x86' if arch == '32' else 'x64'})\n"
    
    for pkg_name, pkg_version in packages:
        formatted_version = get_formatted_python_version(pkg_version)
        if pkg_version:
            content += f'Source: "{{#SourceDirectoryGdb{arch}Python{formatted_version}}}\\*"; '
            content += f'DestDir: "{{code:GetApplicationToolchainSuperHPath}}"; '
            content += f'Flags: ignoreversion recursesubdirs createallsubdirs; '
            content += f'Components: "main\\gdb\\{arch}\\python{formatted_version}"\n'
        else:
            content += f'Source: "{{#SourceDirectoryGdb{arch}}}\\*"; '
            content += f'DestDir: "{{code:GetApplicationToolchainSuperHPath}}"; '
            content += f'Flags: ignoreversion recursesubdirs createallsubdirs; '
            content += f'Components: "main\\gdb\\{arch}\\nopython"\n'
    
    return content

def generate_component_entries(seven_zip_path, packages, arch, packages_path):
    """
    Generate the [Components] section entries for the specified architecture.
    
    Args:
        seven_zip_path (str): Path to 7z.exe    
        packages (list): List of packages (name, version) tuples
        arch (str): Architecture type ('32' or '64')
        packages_path (str): Path where are located binary packages for GDB
        
    Returns:
        str: Component entries as text
    """
    arch_desc = "x86" if arch == "32" else "x64"
    content = f"; {arch}-bit ({arch_desc})\n"
    content += f'Name: "main\\gdb\\{arch}"; Description: "{{cm:ComponentGdb{arch}}}"; Flags: fixed\n'
    
    for pkg_name, pkg_version in packages:
        formatted_version = get_formatted_python_version(pkg_version)
        pkg_total_size = calculate_package_size(seven_zip_path, packages_path, pkg_version)        
        if pkg_version:
            content += f'Name: "main\\gdb\\{arch}\\python{formatted_version}"; '
            content += f'Description: "{{cm:GdbPython_{formatted_version}}}"; '            
        else:
            content += f'Name: "main\\gdb\\{arch}\\nopython"; '
            content += f'Description: "{{cm:GdbPython_None}}"; '
        content += f'ExtraDiskSpaceRequired: {pkg_total_size}; '
        content += f'Flags: exclusive fixed\n'
    
    return content

def generate_gdb_config(version_32bit, version_64bit, path_32bit, path_64bit, seven_zip_path):
    """
    Generate GDB configuration file based on versions and packages found in the specified paths.
    
    Args:
        version_32bit (str): Version for GDB 32-bit (e.g., "10.2")
        version_64bit (str): Version for GDB 64-bit (e.g., "16.2")
        path_32bit (str): Path to 32-bit packages directory
        path_64bit (str): Path to 64-bit packages directory
        seven_zip_path (str): Path to 7z.exe        
        
    Returns:
        str: Generated configuration file content
    """
    # Scan for packages
    packages_32bit = scan_packages(path_32bit)
    packages_64bit = scan_packages(path_64bit)
    
    # Get current date and time
    from datetime import datetime
    current_datetime = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
    # Header
    content = """// =============================================================================
//  ____                         _____  ____   _____
// |    .  ___  ___  ___  _____ |   __||    . |  |  |
// |  |  ||  _|| -_|| .'||     ||__   ||  |  ||    -|
// |____/ |_|  |___||__,||_|_|_||_____||____/ |__|__|
//
// =============================================================================
// DreamSDK Setup - GDB Configuration
// =============================================================================
//
// THIS FILE HAS BEEN GENERATED ON """ + current_datetime + """.
// PLEASE DON'T UPDATE IT.

"""

    # Architecture sections
    for arch, version, packages in [("32", version_32bit, packages_32bit), ("64", version_64bit, packages_64bit)]:
        content += f"; {arch}-bit ({'x86' if arch == '32' else 'x64'})\n"
        content += f"#define Gdb{arch}Version \"{version}\"\n"
        content += f"#define Gdb{arch}Count {len(packages)}\n\n"
        content += generate_source_directories(packages, arch)
        content += "\n"
    
    # Custom messages
    content += generate_custom_messages(packages_32bit, packages_64bit) + "\n"
    
    # Code section
    content += "[Code]\n"
    content += "procedure InitializeArrayGdb();\n"
    content += "begin\n"
    
    # Initialize packages
    for i, (arch, packages) in enumerate([("32", packages_32bit), ("64", packages_64bit)]):
        content += generate_package_initializations(packages, arch)
        # Add a newline between architectures but not after the last one
        if i < 1 and packages_64bit:  # Only add newline if there are 64-bit packages
            content += "\n"
    
    content += "end;\n\n"
    
    # Files section
    content += "[Files]\n"
    
    # File entries
    for arch, packages in [("32", packages_32bit), ("64", packages_64bit)]:
        content += generate_file_entries(packages, arch)
        content += "\n"
    
    # Components section
    content += "[Components]\n"
    
    # Component entries
    for arch, packages, packages_path in [("32", packages_32bit, path_32bit), ("64", packages_64bit, path_64bit)]:
        content += generate_component_entries(seven_zip_path, packages, arch, packages_path)
        if arch == "32":  # Add extra newline between 32-bit and 64-bit components
            content += "\n"
    
    return content

def main():
    """
    Main function that processes command line arguments and generates the file.
    """
    if len(sys.argv) != 7:
        print("Usage: python mkcfggdb.py <output_path> <32bit_version> <64bit_version> <32bit_path> <64bit_path> <7z_path>")
        print("Example: python mkcfggdb.py /path/to/output 10.2 16.2 /path/to/32bit /path/to/64bit /path/to/7z.exe")
        sys.exit(1)

    path_output = sys.argv[1]        
    version_32bit = sys.argv[2]
    version_64bit = sys.argv[3]
    path_32bit = sys.argv[4]
    path_64bit = sys.argv[5]
    seven_zip_path = sys.argv[6]
    
    if not os.path.exists(seven_zip_path):
        print(f"Error: 7-Zip executable {seven_zip_path} does not exist")
        sys.exit(1)    
    
    # Generate content
    content = generate_gdb_config(version_32bit, version_64bit, path_32bit, path_64bit, seven_zip_path)
    
    # Write to output file    
    full_output_path = os.path.join(path_output, 'gdb.context.iss')
    with open(full_output_path, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print(f"GDB configuration file successfully generated at: {full_output_path}")

if __name__ == "__main__":
    main()
