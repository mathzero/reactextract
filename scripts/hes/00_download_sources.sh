#!/bin/sh
# Download the public NHS England files behind the HES profile into
# scripts/hes/sources/ and check them against the recorded SHA-256 checksums.
# Run from the package root: sh scripts/hes/00_download_sources.sh
set -eu

dir="data-raw/sources"
mkdir -p "$dir"

fetch() {
  url="$1"; file="$2"; sum="$3"
  if [ ! -f "$dir/$file" ]; then
    curl -sSfL -o "$dir/$file" "$url"
  fi
  echo "$sum  $dir/$file" | shasum -a 256 -c -
}

base="https://files.digital.nhs.uk/assets/Services/Artificial%20data/Artificial%20HES%20final"
fetch "$base/artificial_hes_apc_202302_v1_sample.zip" artificial_hes_apc_202302_v1_sample.zip \
  d0136b7987fb70c5938ab36835e8c1095d31fe8ac30ad15358797a947ddc6779
fetch "$base/artificial_hes_op_202302_v1_sample.zip" artificial_hes_op_202302_v1_sample.zip \
  6cccd1c510e4534a4a8644da0cbd9cd029d3ea80b2e417efdbbf57d2c5c06272
fetch "https://files.digital.nhs.uk/7B/BCBF63/hosp-epis-stat-admi-diag-2023-24.csv" \
  hosp-epis-stat-admi-diag-2023-24.csv \
  21a6654347036da041f99dfe6e0bbe99a8ef666716867a39d7c2e2456078d052
fetch "https://files.digital.nhs.uk/0B/553EEF/hosp-epis-stat-admi-proc-2023-24.csv" \
  hosp-epis-stat-admi-proc-2023-24.csv \
  e1abbc7f3e81369a3674c1241ec0e2e733d8b31fa2c8a99991a26e39d770c02d
