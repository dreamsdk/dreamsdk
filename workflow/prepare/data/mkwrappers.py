#!/usr/bin/env python3
"""
mkwrappers.py - Script to generate 7z archives with DreamSDK wrappers.

This script creates two 7z archives:
- arm-eabi-wrappers-bin.7z (for dc-arm-* files)
- sh-elf-wrappers-bin.7z (for kos-* files)

Each archive contains copies of the dreamsdk_runner executable renamed according to
the wrapper files found in the build_wrappers directory, along with corresponding
.cfg files containing the path configuration.
"""

import os
import sys
import shutil
import tempfile
import subprocess
from pathlib import Path


def create_7z_archive(archive_name, temp_dir, sevenzip_path, output_path):
    """Create a 7z archive from a temporary directory."""
    try:
        # Create full path for the archive in the output directory
        archive_path = Path(output_path) / archive_name
        
        # Use 7z command line tool to create the archive
        cmd = [sevenzip_path, 'a', str(archive_path), f'{temp_dir}/*']
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        print(f"Successfully created {archive_path}")
        return True
    except subprocess.CalledProcessError as e:
        print(f"Error creating {archive_path}: {e}")
        print(f"stdout: {e.stdout}")
        print(f"stderr: {e.stderr}")
        return False
    except FileNotFoundError:
        print(f"Error: 7z executable not found at {sevenzip_path}. Please check the path.")
        return False


def process_wrapper_files(input_kos_path, dreamsdk_runner, sevenzip_path, output_path):
    """
    Process wrapper files and create the 7z archives.
    
    Args:
        input_kos_path: Path to the KOS directory
        dreamsdk_runner: Path to the dreamsdk_runner executable
        sevenzip_path: Path to the 7z executable
        output_path: Path where the archives will be created
    """
    # Define paths
    build_wrappers_dir = Path(input_kos_path) / "utils" / "build_wrappers"
    dreamsdk_runner_path = Path(dreamsdk_runner)
    output_dir = Path(output_path)
    
    # Check if paths exist
    if not build_wrappers_dir.exists():
        print(f"Error: Directory {build_wrappers_dir} does not exist")
        return False
    
    if not dreamsdk_runner_path.exists():
        print(f"Error: File {dreamsdk_runner_path} does not exist")
        return False
    
    # Check if 7z executable exists
    sevenzip_path_obj = Path(sevenzip_path)
    if not sevenzip_path_obj.exists():
        print(f"Error: 7z executable {sevenzip_path_obj} does not exist")
        return False
    
    # Create output directory if it doesn't exist
    try:
        output_dir.mkdir(parents=True, exist_ok=True)
    except OSError as e:
        print(f"Error creating output directory {output_dir}: {e}")
        return False
    
    # Get all files in build_wrappers directory
    try:
        wrapper_files = [f.name for f in build_wrappers_dir.iterdir() if f.is_file()]
    except OSError as e:
        print(f"Error reading directory {build_wrappers_dir}: {e}")
        return False
    
    # Separate dc-arm and kos files
    dc_arm_files = [f for f in wrapper_files if f.startswith('dc-arm-')]
    kos_files = [f for f in wrapper_files if f.startswith('kos-')]
    
    print(f"Found {len(dc_arm_files)} dc-arm- files: {dc_arm_files}")
    print(f"Found {len(kos_files)} kos- files: {kos_files}")
    
    # Configuration file content
    cfg_content = "/opt/toolchains/dc/kos/utils/build_wrappers/"
    
    success = True
    
    # Process dc-arm files (arm-eabi archive)
    if dc_arm_files:
        with tempfile.TemporaryDirectory() as temp_dir:
            arm_eabi_dir = Path(temp_dir) / "arm-eabi/bin"
            arm_eabi_dir.mkdir(parents=True, exist_ok=True)
            
            for filename in dc_arm_files:
                # Copy dreamsdk_runner as .exe file
                exe_path = arm_eabi_dir / f"{filename}.exe"
                shutil.copy2(dreamsdk_runner_path, exe_path)
                
                # Create .cfg file
                cfg_path = arm_eabi_dir / f"{filename}.cfg"
                with open(cfg_path, 'w', encoding='utf-8') as cfg_file:
                    cfg_file.write(cfg_content)
                
                print(f"Created {exe_path.name} and {cfg_path.name}")
            
            # Create 7z archive
            if not create_7z_archive("arm-eabi-wrappers-bin.7z", temp_dir, sevenzip_path, output_path):
                success = False
    
    # Process kos files (sh-elf archive)
    if kos_files:
        with tempfile.TemporaryDirectory() as temp_dir:
            sh_elf_dir = Path(temp_dir) / "sh-elf/bin"
            sh_elf_dir.mkdir(parents=True, exist_ok=True)
            
            for filename in kos_files:
                # Copy dreamsdk_runner as .exe file
                exe_path = sh_elf_dir / f"{filename}.exe"
                shutil.copy2(dreamsdk_runner_path, exe_path)
                
                # Create .cfg file
                cfg_path = sh_elf_dir / f"{filename}.cfg"
                with open(cfg_path, 'w', encoding='utf-8') as cfg_file:
                    cfg_file.write(cfg_content)
                
                print(f"Created {exe_path.name} and {cfg_path.name}")
            
            # Create 7z archive
            if not create_7z_archive("sh-elf-wrappers-bin.7z", temp_dir, sevenzip_path, output_path):
                success = False
    
    return success


def main():
    """Main function to handle command line arguments and execute the script."""
    if len(sys.argv) != 5:
        print("Usage: python script.py <input_kos_path> <dreamsdk_runner> <7z_path> <output_path>")
        print("")
        print("Arguments:")
        print("  input_kos_path   : Path to the KOS directory containing utils/build_wrappers")
        print("  dreamsdk_runner  : Path to the dreamsdk_runner executable")
        print("  7z_path          : Path to the 7z.exe executable")
        print("  output_path      : Path where to store the generated packages")
        print("")
        print("Example:")
        print("  python script.py C:/path/to/kos C:/path/to/dreamsdk_runner.exe C:/path/to/7z.exe")
        sys.exit(1)
    
    input_kos_path = sys.argv[1]
    dreamsdk_runner = sys.argv[2]
    sevenzip_path = sys.argv[3]
    output_path = sys.argv[4]
    
    print(f"Input KOS directory: {input_kos_path}")
    print(f"DreamSDK runner: {dreamsdk_runner}")
    print(f"7z executable: {sevenzip_path}")
    print(f"Output directory: {output_path}")
    print("")
    
    if process_wrapper_files(input_kos_path, dreamsdk_runner, sevenzip_path, output_path):
        print("\nAll archives created successfully!")
        sys.exit(0)
    else:
        print("\nSome errors occurred during archive creation.")
        sys.exit(1)


if __name__ == "__main__":
    main()