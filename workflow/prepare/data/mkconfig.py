#!/usr/bin/env python3
"""
INI to CONF Configuration Generator
Usage: python script.py <config_ini_path> <input32_path> <input64_path> <output32_path> <output64_path> <7z_path>

This script reads an INI configuration file and scans directories for 7z packages
to generate separate .conf files for 32-bit and 64-bit architectures with checksums.
"""

import sys
import os
import logging
import configparser
import hashlib
import re
import tempfile
import shutil
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import subprocess

def extract_python_version_from_filename(filename: str) -> Optional[str]:
    """Extract Python version from GDB package filename."""
    patterns = [
        r'sh-elf-gdb-python-(\d+\.\d+)-bin\.7z',
        r'sh-elf-gdb-no-python-bin\.7z'
    ]
    
    for pattern in patterns:
        match = re.match(pattern, filename)
        if match:
            if 'no-python' in filename:
                return 'no-python'
            else:
                return match.group(1)
    return None

def python_version_to_profile_name(version: str) -> str:
    """Convert Python version to profile name format."""
    if version == 'no-python':
        return 'NOPYTHON'
    else:
        # Convert "2.7" to "PYTHON27", "3.10" to "PYTHON310", etc.
        return f"PYTHON{version.replace('.', '')}"

def calculate_md5_checksum(file_path: str) -> str:
    """Calculate MD5 checksum of a file."""
    hash_md5 = hashlib.md5()
    try:
        with open(file_path, "rb") as f:
            for chunk in iter(lambda: f.read(4096), b""):
                hash_md5.update(chunk)
        return hash_md5.hexdigest()
    except Exception as e:
        print(f"Warning: Could not calculate checksum for {file_path}: {e}")
        return "unknown"

def get_gcc_version_from_archive(archive_path: str, sevenzip_path: str) -> Optional[str]:
    """Extract GCC version by running sh-elf-gcc --version from archive."""
    try:
        with tempfile.TemporaryDirectory() as temp_dir:
            # Extract 7z archive
            result = subprocess.run([
                sevenzip_path, 'x', archive_path, '-o' + temp_dir, '-y'
            ], capture_output=True, text=True)
            
            if result.returncode != 0:
                print(f"Warning: Failed to extract {archive_path}")
                return None
            
            # Look for the GCC executable
            gcc_path = os.path.join(temp_dir, 'sh-elf', 'bin', 'sh-elf-gcc.exe')
            if os.path.exists(gcc_path):
                # Run sh-elf-gcc --version
                result = subprocess.run([gcc_path, '--version'], 
                                      capture_output=True, text=True)
                if result.returncode == 0:
                    # Parse version from output (first line usually contains version)
                    version_line = result.stdout.split('\n')[0]
                    # Extract version number (e.g., "sh-elf-gcc (GCC) 9.2.0" -> "9.2.0")
                    version_match = re.search(r'(\d+\.\d+(?:\.\d+)?)', version_line)
                    if version_match:
                        return version_match.group(1)
                else:
                    print(f"Warning: Failed to run --version on {gcc_path}")
            else:
                print(f"Warning: sh-elf-gcc.exe not found in {archive_path}")
                
    except Exception as e:
        print(f"Error getting GCC version from {archive_path}: {e}")
    
    return None

def extract_7z_and_get_checksum(archive_path: str, target_executable: str, sevenzip_path: str) -> Optional[str]:
    """Extract 7z archive and calculate checksum of target executable."""
    try:
        # Create temporary directory
        with tempfile.TemporaryDirectory() as temp_dir:
            # Extract 7z archive
            result = subprocess.run([
                sevenzip_path, 'x', archive_path, '-o' + temp_dir, '-y'
            ], capture_output=True, text=True)
            
            if result.returncode != 0:
                print(f"Warning: Failed to extract {archive_path}")
                return None
            
            # Look for the target executable
            executable_path = os.path.join(temp_dir, 'sh-elf', 'bin', target_executable)
            if os.path.exists(executable_path):
                return calculate_md5_checksum(executable_path)
            else:
                print(f"Warning: Executable {target_executable} not found in {archive_path}")
                return None
                
    except Exception as e:
        print(f"Error processing {archive_path}: {e}")
        return None

def scan_directory_for_packages(directory: str, architecture: str, sevenzip_path: str) -> Dict[str, Dict]:
    """Scan directory for 7z packages and collect information."""
    logger = logging.getLogger(__name__)
    
    packages_info = {
        'gdb_packages': {},
        'toolchain_packages': {}
    }
    
    if not os.path.exists(directory):
        print(f"Warning: Directory {directory} does not exist")
        return packages_info
    
    print(f"Scanning directory for {architecture}-bit: {directory}")
    
    for filename in os.listdir(directory):
        if not filename.endswith('.7z'):
            continue
            
        full_path = os.path.join(directory, filename)
        logger.debug(f"Found file: {filename}")  # Debug: show all .7z files
        
        # Process GDB packages
        if filename.startswith('sh-elf-gdb-'):
            python_version = extract_python_version_from_filename(filename)
            if python_version:
                print(f"Processing GDB package: {filename} (Python {python_version})")
                checksum = extract_7z_and_get_checksum(full_path, 'sh-elf-gdb.exe', sevenzip_path)
                profile_name = python_version_to_profile_name(python_version)
                
                packages_info['gdb_packages'][profile_name] = {
                    'filename': filename,
                    'checksum': checksum or 'unknown',
                    'python_version': python_version,
                    'package_name': filename.replace('-bin.7z', ''),
                    'architecture': architecture
                }
            else:
                print(f"Skipping GDB package (no Python version detected): {filename}")
        
        # Process toolchain packages - amélioration de la détection
        elif filename.startswith('sh-elf-') and not filename.startswith('sh-elf-gdb-'):
            print(f"Processing potential toolchain package: {filename}")
            
            # Try to extract and get checksum
            checksum = extract_7z_and_get_checksum(full_path, 'sh-elf-gcc.exe', sevenzip_path)
            
            if checksum and checksum != 'unknown':
                # Get GCC version from the archive
                gcc_version = get_gcc_version_from_archive(full_path, sevenzip_path)
                
                packages_info['toolchain_packages'][filename] = {
                    'filename': filename,
                    'checksum': checksum,
                    'gcc_version': gcc_version,
                    'architecture': architecture
                }
                print(f"Successfully processed toolchain package: {filename} (GCC {gcc_version})")
            else:
                print(f"Failed to process toolchain package: {filename} (checksum calculation failed)")
        
        # Also check for other potential toolchain patterns
        elif ('gcc' in filename.lower() and 
              filename.endswith('.7z') and 
              not filename.startswith('arm-eabi') and
              not filename.startswith('sh-elf-gdb')):
            print(f"Processing alternative toolchain package pattern: {filename}")
            
            # Try to extract and get checksum
            checksum = extract_7z_and_get_checksum(full_path, 'sh-elf-gcc.exe', sevenzip_path)
            
            if checksum and checksum != 'unknown':
                gcc_version = get_gcc_version_from_archive(full_path, sevenzip_path)
                
                packages_info['toolchain_packages'][filename] = {
                    'filename': filename,
                    'checksum': checksum,
                    'gcc_version': gcc_version,
                    'architecture': architecture
                }
                print(f"Successfully processed alternative toolchain package: {filename} (GCC {gcc_version})")
            else:
                print(f"Failed to process alternative toolchain package: {filename}")
        else:
            logger.debug(f"Skipping file: {filename} (doesn't match any pattern)")
    
    logger.debug(f"Found {len(packages_info['toolchain_packages'])} toolchain packages:")
    for filename, info in packages_info['toolchain_packages'].items():
        logger.debug(f"  - {filename}: GCC {info['gcc_version']}, checksum: {info['checksum'][:8]}...")
    
    return packages_info
    
def parse_ini_config(config_path: str) -> Dict:
    """Parse INI configuration file."""
    config = configparser.ConfigParser()
    # Preserve the case of option names (keys)
    config.optionxform = str
    config.read(config_path, encoding='utf-8')
    
    parsed_data = {}
    for section_name in config.sections():
        section_data = {}
        for key, value in config.items(section_name):
            # Handle semicolon-separated values
            if ';' in value and ('VERSIONS' in key.upper()):
                section_data[key] = [item.strip() for item in value.split(';')]
            else:
                section_data[key] = value
        parsed_data[section_name] = section_data
    
    return parsed_data

def extract_toolchain_profiles_from_ini(ini_data: Dict, architecture: str) -> Dict:
    """Extract toolchain profiles from INI data."""
    logger = logging.getLogger(__name__)
    
    profiles = {}
    
    # Get versions from main Toolchains section
    toolchain_section = "Toolchains"
    if toolchain_section not in ini_data:
        print(f"Warning: Section [{toolchain_section}] not found in INI")
        return profiles
    
    main_section = ini_data[toolchain_section]
    
    # Find profile versions for this architecture
    versions_key = f'TOOLCHAINS{architecture}_VERSIONS'
    if versions_key not in main_section:
        print(f"Warning: Key {versions_key} not found in [{toolchain_section}] section")
        print(f"Available keys: {list(main_section.keys())}")
        return profiles
    
    versions_value = main_section[versions_key]
    if isinstance(versions_value, list):
        profile_versions = versions_value
    else:
        profile_versions = [v.strip() for v in versions_value.split(';')]
    
    logger.debug(f"Found toolchain versions for {architecture}-bit: {profile_versions}")
    
    # Get profile details from architecture-specific section
    arch_section_name = f"Toolchains{architecture}"
    if arch_section_name not in ini_data:
        print(f"Warning: Section [{arch_section_name}] not found in INI")
        return profiles
    
    arch_section = ini_data[arch_section_name]
    logger.debug(f"Available keys in [{arch_section_name}]: {list(arch_section.keys())}")
    
    # Extract profile information
    for version in profile_versions:
        version = version.strip()
        profile_info = {}
        
        # Get name (utilise la casse exacte du fichier INI)
        name_key = f"TOOLCHAINS{architecture}_VERSION_NAME_{version}"
        if name_key in arch_section:
            profile_info['name'] = arch_section[name_key]
        else:
            print(f"Warning: Name key {name_key} not found")
        
        # Get description
        desc_key = f"TOOLCHAINS{architecture}_VERSION_DESC_{version}"
        if desc_key in arch_section:
            profile_info['description'] = arch_section[desc_key]
        else:
            print(f"Warning: Description key {desc_key} not found")
        
        # Get packages
        arm_key = f"TOOLCHAINS{architecture}_VERSION_PACKAGE_ARMEABI_{version}"
        if arm_key in arch_section:
            profile_info['arm_eabi_package'] = arch_section[arm_key]
        else:
            print(f"Warning: ARM EABI package key {arm_key} not found")
        
        shelf_key = f"TOOLCHAINS{architecture}_VERSION_PACKAGE_SHELF_{version}"
        if shelf_key in arch_section:
            profile_info['sh_elf_package'] = arch_section[shelf_key]
        else:
            print(f"Warning: SH-ELF package key {shelf_key} not found")
        
        if profile_info:
            profiles[version] = profile_info
            print(f"Successfully extracted profile {version}: {profile_info.get('name', 'Unknown')}")
    
    return profiles

def match_toolchain_checksums_and_versions(toolchain_profiles: Dict, scanned_packages: Dict) -> Tuple[Dict, Dict, Dict]:
    """Match toolchain profiles with their checksums and versions from scanned packages."""
    logger = logging.getLogger(__name__)
    
    checksum_map = {}
    version_map = {}
    package_name_map = {}  # Maps profile_id to actual package names
    
    logger.debug(f"Matching toolchain profiles with scanned packages...")
    logger.debug(f"Toolchain profiles: {list(toolchain_profiles.keys())}")
    logger.debug(f"Scanned packages: {list(scanned_packages['toolchain_packages'].keys())}")
    
    for profile_id, profile_info in toolchain_profiles.items():
        logger.debug(f"Processing profile '{profile_id}'")
        
        # Generate search patterns from profile_id
        profile_patterns = [profile_id.lower()]
        
        # If profile_id contains numbers and letters, try variations
        if re.match(r'^\d+.*', profile_id):
            # For profiles like "950WINXP" -> ["950winxp", "950", "winxp"]
            profile_patterns.append(profile_id.lower())
            # Extract just the numbers
            numbers = re.findall(r'\d+', profile_id)
            if numbers:
                profile_patterns.extend(numbers)
            # Extract just the letters
            letters = re.findall(r'[a-zA-Z]+', profile_id)
            if letters:
                profile_patterns.extend([letter.lower() for letter in letters])
        
        logger.debug(f"Generated search patterns for '{profile_id}': {profile_patterns}")
        
        # Try to find matching package in scanned packages
        found_match = False
        best_match = None
        best_match_score = 0
        
        for filename, package_info in scanned_packages['toolchain_packages'].items():
            # Remove the -bin.7z suffix from the filename to get the package name
            package_name = filename.replace('-bin.7z', '')
            
            logger.debug(f"Checking package: {package_name}")
            
            # Calculate match score based on pattern matches
            match_score = 0
            matched_patterns = []
            
            for pattern in profile_patterns:
                if pattern in package_name.lower():
                    match_score += len(pattern)  # Longer matches get higher score
                    matched_patterns.append(pattern)
            
            # Bonus points for GCC version match
            if package_info.get('gcc_version') and 'name' in profile_info:
                pkg_gcc_version = package_info['gcc_version']
                profile_name = profile_info['name']
                
                # Try to extract expected GCC version from profile name
                version_match = re.search(r'(\d+\.\d+(?:\.\d+)?)', profile_name)
                if version_match:
                    expected_gcc_version = version_match.group(1)
                    if pkg_gcc_version.startswith(expected_gcc_version):
                        match_score += 100  # High bonus for version match
                        matched_patterns.append(f"gcc-{expected_gcc_version}")
                        logger.debug(f"GCC version bonus: {pkg_gcc_version} matches expected {expected_gcc_version}")
            
            if match_score > best_match_score:
                best_match_score = match_score
                best_match = (filename, package_info)
                logger.debug(f"New best match for '{profile_id}': {package_name} (score: {match_score}, patterns: {matched_patterns})")
        
        # Use the best match if we found one
        if best_match and best_match_score > 0:
            filename, package_info = best_match
            package_name = filename.replace('-bin.7z', '')
            
            logger.debug(f"MATCH FOUND! Profile '{profile_id}' matches '{filename}' (score: {best_match_score})")
            
            # Store checksum mapping
            if package_info['checksum'] != 'unknown':
                checksum_map[package_info['checksum']] = profile_id
                logger.debug(f"Added checksum mapping: {package_info['checksum']} -> {profile_id}")
            
            # Store version mapping (use the actual GCC version from the package)
            if package_info['gcc_version']:
                version_map[profile_id] = package_info['gcc_version']
                logger.debug(f"Added version mapping: {profile_id} -> {package_info['gcc_version']}")
            
            # Store actual package names based on the found files
            sh_elf_name = package_name
            # Generate ARM EABI name by replacing sh-elf-toolchain with arm-eabi-toolchain
            arm_eabi_name = package_name.replace('sh-elf-toolchain', 'arm-eabi-toolchain')
            
            package_name_map[profile_id] = {
                'sh_elf': sh_elf_name,
                'arm_eabi': arm_eabi_name
            }
            logger.debug(f"Added package names: {profile_id} -> SH-ELF: {sh_elf_name}, ARM-EABI: {arm_eabi_name}")
            
            found_match = True
        
        if not found_match:
            print(f"WARNING: No matching package found for profile '{profile_id}'")
            
            # Use INI information as fallback
            sh_elf_package = profile_info.get('sh_elf_package', '')
            arm_eabi_package = profile_info.get('arm_eabi_package', '')
            package_name_map[profile_id] = {
                'sh_elf': sh_elf_package,
                'arm_eabi': arm_eabi_package
            }
            logger.debug(f"Using INI fallback for '{profile_id}' -> SH-ELF: {sh_elf_package}, ARM-EABI: {arm_eabi_package}")
    
    logger.debug(f"Final checksum_map: {checksum_map}")
    logger.debug(f"Final version_map: {version_map}")
    logger.debug(f"Final package_name_map: {package_name_map}")
    return checksum_map, version_map, package_name_map

def generate_conf_file(ini_data: Dict, packages_info: Dict, architecture: str) -> str:
    """Generate .conf file content for specific architecture."""
    conf_lines = []
    
    # Extract toolchain profiles
    toolchain_profiles = extract_toolchain_profiles_from_ini(ini_data, architecture)
    
    # Header
    conf_lines.append(f"# Generated {architecture}-bit configuration file")
    conf_lines.append("# Contains toolchain and GDB package information with checksums")
    conf_lines.append("")
    
    # [Packages] section
    conf_lines.append("[Packages]")
    
    # ToolchainProfiles
    toolchain_profile_names = list(toolchain_profiles.keys())
    if toolchain_profile_names:
        conf_lines.append(f"ToolchainProfiles={';'.join(toolchain_profile_names)}")
    
    # GdbProfiles
    gdb_profile_names = sorted(packages_info['gdb_packages'].keys())
    if gdb_profile_names:
        conf_lines.append(f"GdbProfiles={';'.join(gdb_profile_names)}")
    
    conf_lines.append("")
    
    # Get toolchain checksums, versions, and actual package names
    toolchain_checksums, toolchain_versions, package_names = match_toolchain_checksums_and_versions(toolchain_profiles, packages_info)
    
    # Toolchain Profile sections
    for profile_id, profile_info in toolchain_profiles.items():
        conf_lines.append(f"[ToolchainProfile_{profile_id}]")
        
        if 'name' in profile_info:
            conf_lines.append(f"Name={profile_info['name']}")
        
        # Use the actual GCC version from the package if available
        if profile_id in toolchain_versions:
            conf_lines.append(f"Version={toolchain_versions[profile_id]}")
        else:
            # Fallback: Extract version from name or use profile_id
            if 'name' in profile_info:
                version_match = re.search(r'(\d+\.\d+(?:\.\d+)?)', profile_info['name'])
                version = version_match.group(1) if version_match else profile_id
            else:
                version = profile_id
            conf_lines.append(f"Version={version}")
        
        if 'description' in profile_info:
            conf_lines.append(f"Description={profile_info['description']}")
        
        # Use actual package names if available, otherwise use INI values
        if profile_id in package_names:
            conf_lines.append(f"ArmEabiPackage={package_names[profile_id]['arm_eabi']}")
            conf_lines.append(f"ShElfPackage={package_names[profile_id]['sh_elf']}")
        else:
            # Fallback to INI values
            if 'arm_eabi_package' in profile_info:
                conf_lines.append(f"ArmEabiPackage={profile_info['arm_eabi_package']}")
            if 'sh_elf_package' in profile_info:
                conf_lines.append(f"ShElfPackage={profile_info['sh_elf_package']}")
        
        conf_lines.append("")
    
    # GDB Profile sections
    for profile_name, gdb_info in sorted(packages_info['gdb_packages'].items()):
        conf_lines.append(f"[GdbProfile_{profile_name}]")
        
        # Generate human-readable name
        if gdb_info['python_version'] == 'no-python':
            conf_lines.append("Name=No Python")
        else:
            conf_lines.append(f"Name=Python {gdb_info['python_version']}")
        
        # Get GDB version from INI (default to 10.2)
        gdb_version = "10.2"
        if 'Toolchains' in ini_data:
            gdb_key = f"GDB{architecture}_VERSION"
            if gdb_key in ini_data['Toolchains']:
                gdb_version = ini_data['Toolchains'][gdb_key]
        
        conf_lines.append(f"Version={gdb_version}")
        conf_lines.append(f"Package={gdb_info['package_name']}")
        conf_lines.append("")
    
    # Checksum sections
    conf_lines.append("[ToolchainProfilesChecksums]")
    for checksum, profile_id in toolchain_checksums.items():
        conf_lines.append(f"{checksum}={profile_id}")
    conf_lines.append("")
    
    conf_lines.append("[GdbProfilesChecksums]")
    for profile_name, gdb_info in sorted(packages_info['gdb_packages'].items()):
        if gdb_info['checksum'] != 'unknown':
            conf_lines.append(f"{gdb_info['checksum']}={profile_name}")
    conf_lines.append("")
    
    return '\n'.join(conf_lines)

def check_7z_availability(sevenzip_path: str):
    """Check if 7z command is available."""
    try:
        result = subprocess.run([sevenzip_path], capture_output=True)
        return True
    except FileNotFoundError:
        return False

def main():
    """Main function."""
    if len(sys.argv) != 7:
        print("Usage: python mkconfig.py <config_ini_path> <input32_path> <input64_path> <output32_path> <output64_path> <7z_path>")
        print("  config_ini_path: Path to INI configuration file")
        print("  input32_path: Path to 32-bit packages directory")
        print("  input64_path: Path to 64-bit packages directory")
        print("  output32_path: Output path for 32-bit .conf file")
        print("  output64_path: Output path for 64-bit .conf file")
        print("  7z_path: Path to 7z executable")
        sys.exit(1)
    
    config_ini_path = sys.argv[1]
    input32_path = sys.argv[2]
    input64_path = sys.argv[3]
    output32_path = sys.argv[4]
    output64_path = sys.argv[5]
    sevenzip_path = sys.argv[6]
    
    # Check if INI file exists
    if not os.path.exists(config_ini_path):
        print(f"Error: Configuration file '{config_ini_path}' does not exist.")
        sys.exit(1)
    
    # Check if 7z is available
    if not check_7z_availability(sevenzip_path):
        print(f"Error: 7z command not found at '{sevenzip_path}'. Please provide a valid path to 7z executable.")
        sys.exit(1)
    
    try:
        # Parse INI configuration
        print(f"Reading INI configuration: {config_ini_path}")
        ini_data = parse_ini_config(config_ini_path)
        
        # Scan directories for packages
        print("Scanning for packages and calculating checksums...")
        packages_32bit = scan_directory_for_packages(input32_path, "32", sevenzip_path)
        packages_64bit = scan_directory_for_packages(input64_path, "64", sevenzip_path)
        
        # Generate .conf files
        print("Generating 32-bit configuration file...")
        conf_32bit_content = generate_conf_file(ini_data, packages_32bit, "32")
        
        print("Generating 64-bit configuration file...")
        conf_64bit_content = generate_conf_file(ini_data, packages_64bit, "64")
        
        # Create output directories and save files
        output32_dir = Path(output32_path).parent
        output32_dir.mkdir(parents=True, exist_ok=True)
        
        output64_dir = Path(output64_path).parent
        output64_dir.mkdir(parents=True, exist_ok=True)
        
        # Save 32-bit conf file
        with open(output32_path, 'w', encoding='utf-8') as f:
            f.write(conf_32bit_content)
        print(f"32-bit configuration file generated: {output32_path}")
        
        # Save 64-bit conf file
        with open(output64_path, 'w', encoding='utf-8') as f:
            f.write(conf_64bit_content)
        print(f"64-bit configuration file generated: {output64_path}")
        
        # Print summary
        print("\n=== SUMMARY ===")
        print(f"32-bit packages found:")
        print(f"  - GDB packages: {len(packages_32bit['gdb_packages'])}")
        print(f"  - Toolchain packages: {len(packages_32bit['toolchain_packages'])}")
        print(f"64-bit packages found:")
        print(f"  - GDB packages: {len(packages_64bit['gdb_packages'])}")
        print(f"  - Toolchain packages: {len(packages_64bit['toolchain_packages'])}")
        
    except Exception as e:
        print(f"Error during processing: {e}")
        sys.exit(1)

# Entry point
if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
    main()
