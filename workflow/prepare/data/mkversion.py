#!/usr/bin/env python3
"""
DreamSDK Version Configuration File Generator

This script generates a version.config.iss file with the specified version numbers
and current timestamp.
"""

import os
import sys
from datetime import datetime


def generate_version_config(output_path, major, minor, build):
    """
    Generate a version.config.iss file with the specified parameters.
    
    Args:
        output_path (str): Path where the version.config.iss file will be created
        major (str): Major version number
        minor (str): Minor version number  
        build (str): Build version number
        
    Returns:
        bool: True if file was created successfully, False otherwise
    """
    try:
        # Get current datetime
        current_datetime = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        
        # Create directory if it doesn't exist
        directory = os.path.dirname(output_path)
        if directory and not os.path.exists(directory):
            os.makedirs(directory)
        
        # File content template
        content = f"""; =============================================================================
;  ____                         _____  ____   _____
; |    .  ___  ___  ___  _____ |   __||    . |  |  |
; |  |  ||  _|| -_|| .'||     ||__   ||  |  ||    -|
; |____/ |_|  |___||__,||_|_|_||_____||____/ |__|__|
;
; =============================================================================
; DreamSDK Setup - Version Configuration
; =============================================================================
;
; THIS FILE HAS BEEN GENERATED ON {current_datetime}.
; PLEASE DON'T UPDATE IT.

#define VersionNumberMajor "{major}"
#define VersionNumberMinor "{minor}"
#define VersionNumberBuild "{build}"
"""
        
        # Write the file        
        full_output_path = os.path.join(output_path, 'version.context.iss')
        with open(full_output_path, 'w', encoding='utf-8') as f:
            f.write(content)
        
        print(f"File generated successfully at: {output_path}")
        print(f"Version: {major}.{minor}.{build}")
        print(f"Generated on: {current_datetime}")
        
        return True
        
    except Exception as e:
        print(f"Error: Unable to generate version.config.iss file: {str(e)}")
        return False


def main():
    """
    Main function to handle command line arguments and call the generator.
    """
    if len(sys.argv) != 5:
        print("Usage: python mkversion.py <output_path> <major> <minor> <build>")
        print("Example: python mkversion.py 'C:/temp/version.config.iss' '4' '0' '9'")
        sys.exit(1)
    
    output_path = sys.argv[1]
    major = sys.argv[2]
    minor = sys.argv[3]
    build = sys.argv[4]
    
    # Validate version numbers (should be numeric)
    try:
        int(major)
        int(minor)
        int(build)
    except ValueError:
        print("Error: Version numbers must be numeric")
        sys.exit(1)
    
    # Generate the file
    success = generate_version_config(output_path, major, minor, build)
    
    if not success:
        sys.exit(1)


if __name__ == "__main__":
    main()


# =============================================================================
# Example usage:
# python generate_version.py "version.config.iss" "4" "0" "9"
# 
# Or use the function directly:
# generate_version_config("C:/temp/version.config.iss", "4", "0", "9")
# =============================================================================
