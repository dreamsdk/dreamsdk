#!/usr/bin/env bash

makefile="$1"
variable="$2"

# See: https://stackoverflow.com/a/3352015/3726096
function trim()
{
  local var="$*"
  var="${var#"${var%%[![:space:]]*}"}"
  var="${var%"${var##*[![:space:]]}"}"
  printf '%s' "$var"
}

# Thanks to fedorqui 'SO stop harming'
# See: https://stackoverflow.com/a/39384347/3726096
function get_make_var()
{
  local token="$1"
  local file="$2"
  local result=$(cat ${file} | grep "^[^#;]" | awk -v token="${token}" -F "=" '$0 ~ token {print $2}')
  echo "$(trim ${result})"
}

# Check for parameters
if [ -z "${makefile}" ] || [ -z "${variable}" ]; then
  echo >&2 "usage: <makefile> <variable>";
  exit 1;
fi

# Check for Makefile
if [ ! -f "${makefile}" ]; then
  echo >&2 "file was not found: ${makefile}";
  exit 1;
fi

export MAKEFILE_VARIABLE=`get_make_var ${variable} ${makefile}`;

if [ -z "${MAKEFILE_VARIABLE}" ]; then
  echo >&2 "unknown variable: ${variable}";
  exit 1;    
fi

echo "${MAKEFILE_VARIABLE}";
