#!/usr/bin/env python3
import os
import sys
import shutil
import argparse
from pathlib import Path


def get_next_backup_name(base_path):
    """
    Find the next available backup name.
    If output_dir.old exists, return output_dir.old.000, then .001, etc.
    """
    old_path = f"{base_path}.old"
    
    # If output_dir.old doesn't exist, we can use it
    if not os.path.exists(old_path):
        return old_path
    
    # Otherwise, find the next available number
    counter = 0
    while True:
        numbered_path = f"{old_path}.{counter:03d}"
        if not os.path.exists(numbered_path):
            return numbered_path
        counter += 1


def has_non_empty_files(directory):
    """
    Check if the directory contains non-empty files (size > 0).
    Returns True if at least one file has size > 0.
    """
    for root, dirs, files in os.walk(directory):
        for file in files:
            file_path = os.path.join(root, file)
            try:
                if os.path.getsize(file_path) > 0:
                    return True
            except OSError:
                # In case of file access error, consider it as non-empty
                return True
    return False


def handle_existing_output_dir(output_dir):
    """
    Handle existing output directory according to defined rules.
    """
    if not os.path.exists(output_dir):
        return
    
    print(f"Directory {output_dir} already exists.")
    
    # Check if it contains non-empty files
    if has_non_empty_files(output_dir):
        # Rename the directory
        backup_name = get_next_backup_name(output_dir)
        print(f"Directory contains non-empty files. Renaming to {backup_name}")
        os.rename(output_dir, backup_name)
    else:
        # Remove the directory since it only contains empty files
        print("Directory only contains empty files. Removing.")
        shutil.rmtree(output_dir)


def get_directory_structure(input_dir, max_depth=2):
    """
    Return the directory structure up to max_depth levels.
    Returns a dictionary with directory paths as keys and their subdirectories as values.
    """
    structure = {}
    
    def scan_directory(current_dir, relative_path="", depth=0):
        if depth >= max_depth:
            return
        
        try:
            for item in os.listdir(current_dir):
                item_path = os.path.join(current_dir, item)
                if os.path.isdir(item_path):
                    # Build relative path from input_dir
                    if relative_path:
                        item_relative_path = os.path.join(relative_path, item)
                    else:
                        item_relative_path = item
                    
                    # Add this directory to structure
                    if item_relative_path not in structure:
                        structure[item_relative_path] = []
                    
                    # Recursively scan subdirectories
                    scan_directory(item_path, item_relative_path, depth + 1)
                    
        except OSError as e:
            print(f"Error reading directory {current_dir}: {e}")
    
    scan_directory(input_dir)
    return structure


def create_directory_structure(input_dir, output_dir):
    """
    Create directory structure in output_dir based on input_dir subdirectories up to level 2.
    """
    # Handle existing output directory
    handle_existing_output_dir(output_dir)
    
    # Get directory structure up to level 2
    structure = get_directory_structure(input_dir, max_depth=2)
    
    if not structure:
        print(f"No subdirectories found in {input_dir}")
        return
    
    print(f"Directory structure found: {list(structure.keys())}")
    
    # Create output directory
    os.makedirs(output_dir, exist_ok=True)
    
    # Create each directory with its empty file
    for dir_path in structure.keys():
        # Create full directory path
        full_dir_path = os.path.join(output_dir, dir_path)
        os.makedirs(full_dir_path, exist_ok=True)
        
        # Get just the directory name (last part of the path)
        dir_name = os.path.basename(dir_path)
        
        # Create empty file with same name as directory
        file_path = os.path.join(full_dir_path, dir_name)
        
        # Create empty file
        with open(file_path, 'w') as f:
            pass  # Empty file
        
        print(f"Created: {full_dir_path}/{dir_name}")
    
    print(f"Directory structure successfully created in {output_dir}")


def main():
    parser = argparse.ArgumentParser(
        description="Duplicate directory tree structure with empty files up to level 2"
    )
    parser.add_argument('input_dir', help='Source directory to analyze')
    parser.add_argument('output_dir', help='Destination directory')
    
    args = parser.parse_args()
    
    input_dir = args.input_dir
    output_dir = args.output_dir
    
    # Validations
    if not os.path.exists(input_dir):
        print(f"Error: Source directory '{input_dir}' does not exist.")
        sys.exit(1)
    
    if not os.path.isdir(input_dir):
        print(f"Error: '{input_dir}' is not a directory.")
        sys.exit(1)
    
    print(f"Source directory: {input_dir}")
    print(f"Destination directory: {output_dir}")
    
    try:
        create_directory_structure(input_dir, output_dir)
    except Exception as e:
        print(f"Error creating directory structure: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()