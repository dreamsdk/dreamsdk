#!/usr/bin/env bash

# Generate a valid Volume ID from the "<PackageName>_<ReleaseVersion>" string.
# The volume label is limited to 16 characters (Joliet), so pre-release
# suffixes are shortened if needed, e.g.:
#   DreamSDK_R5-dev-alpha1 => DREAMSDK_R5_D_A1
#   DreamSDK_R5-beta2      => DREAMSDK_R5_B2

MAX_LENGTH=16

# Only d-characters (A-Z, 0-9 and _) are allowed by ISO 9660
volid=$(echo "$1" | tr '[:lower:]' '[:upper:]' | sed 's/[^A-Z0-9_]/_/g')

# Shorten the pre-release suffixes (e.g., "_ALPHA1" => "_A1")
if [ ${#volid} -gt $MAX_LENGTH ]; then
  volid=$(echo "$volid" | sed -e 's/_ALPHA/_A/g' -e 's/_BETA/_B/g' -e 's/_DEV/_D/g')
fi

# Still too long: remove the separators in the suffix (e.g., "DREAMSDK_R5DA1")
if [ ${#volid} -gt $MAX_LENGTH ]; then
  prefix=$(echo "$volid" | cut -d_ -f1-2)
  suffix=$(echo "$volid" | cut -d_ -f3- -s | tr -d '_')
  volid="${prefix}${suffix}"
fi

echo "${volid:0:$MAX_LENGTH}"
