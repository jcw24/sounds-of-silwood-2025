import os
import subprocess
import re
from datetime import datetime

def extract_datetime_from_filename(filename):
    """
    Extract date and time from audio filename.
    Handles multiple formats:
    - YYYYMMDD_HHMMSS_Edited.wav
    - PREFIX_YYYYMMDD_HHMMSS.wav (like 2MM06276_20250508_000000.wav)
    - Other common timestamp formats
    """
    # Remove file extension
    base_name = os.path.splitext(filename)[0]
    
    # Pattern 1: Files with prefix like 2MM06276_20250508_000000
    prefix_pattern = r'^[^_]*_(\d{8})_(\d{6})$'
    match = re.match(prefix_pattern, base_name)
    
    if match:
        date_str = match.group(1)
        time_str = match.group(2)
        
        try:
            year = int(date_str[:4])
            month = int(date_str[4:6])
            day = int(date_str[6:8])
            hour = int(time_str[:2])
            minute = int(time_str[2:4])
            second = int(time_str[4:6])
            
            # Create datetime object
            dt = datetime(year, month, day, hour, minute, second)
            return dt.strftime('%Y-%m-%d'), dt.strftime('%H:%M:%S')
            
        except ValueError:
            pass
    
    # Pattern 2: Original format YYYYMMDD_HHMMSS_Edited
    edited_pattern = r'^(\d{8})_(\d{6})_Edited$'
    match = re.match(edited_pattern, base_name)
    
    if match:
        date_str = match.group(1)
        time_str = match.group(2)
        
        try:
            year = int(date_str[:4])
            month = int(date_str[4:6])
            day = int(date_str[6:8])
            hour = int(time_str[:2])
            minute = int(time_str[2:4])
            second = int(time_str[4:6])
            
            # Create datetime object
            dt = datetime(year, month, day, hour, minute, second)
            return dt.strftime('%Y-%m-%d'), dt.strftime('%H:%M:%S')
            
        except ValueError:
            pass
    
    # Other potential patterns
    fallback_patterns = [
        # Any YYYYMMDD_HHMMSS pattern anywhere in filename
        r'(\d{8})[_-](\d{6})',
        # YYYY-MM-DD_HH-MM-SS or similar
        r'(\d{4})[_-](\d{2})[_-](\d{2})[_T\s](\d{2})[_:-](\d{2})[_:-](\d{2})',
        # Just YYYYMMDD_HHMMSS at start
        r'^(\d{8})_(\d{6})',
    ]
    
    for pattern in fallback_patterns:
        match = re.search(pattern, base_name)
        if match:
            groups = match.groups()
            
            try:
                if len(groups) == 2:  # YYYYMMDD_HHMMSS format
                    date_str = groups[0]
                    time_str = groups[1]
                    year = int(date_str[:4])
                    month = int(date_str[4:6])
                    day = int(date_str[6:8])
                    hour = int(time_str[:2])
                    minute = int(time_str[2:4])
                    second = int(time_str[4:6])
                    
                elif len(groups) == 6:  # YYYY-MM-DD HH-MM-SS format
                    year = int(groups[0])
                    month = int(groups[1])
                    day = int(groups[2])
                    hour = int(groups[3])
                    minute = int(groups[4])
                    second = int(groups[5])
                
                # Create datetime object
                dt = datetime(year, month, day, hour, minute, second)
                return dt.strftime('%Y-%m-%d'), dt.strftime('%H:%M:%S')
                
            except (ValueError, IndexError):
                continue
    
    # If no pattern matches, return empty strings
    return '', ''

# Folder containing your site folders
base_audio_folder = r\"C:\path-to-audio-files"
# Output base folder
output_base = r\"C:\path-to-output-files"

# Your location info and parameters
latitude = "51.413911"
longitude = "-0.651843"
week = "13"
sensitivity = "1.25"

# Create output directory
os.makedirs(output_base, exist_ok=True)

# Track if header is written
header_written = False
site_files_processed = 0

# Open the master combined file for writing
master_txt_path = os.path.join(output_base, "All_Sites_Combined.txt")

with open(master_txt_path, 'w', encoding='utf-8') as master_file:
    
    # Loop through each site folder
    for site in os.listdir(base_audio_folder):
        site_path = os.path.join(base_audio_folder, site)
        if os.path.isdir(site_path):
            out_path = os.path.join(output_base, site)
            os.makedirs(out_path, exist_ok=True)
            print(f"Processing site: {site}")
            
            # Run BirdNET Analyzer
            cmd = [
                "python", "-m", "birdnet_analyzer.analyze",
                site_path,
                "--output", out_path,
                "--lat", latitude,
                "--lon", longitude,
                "--week", week,
                "--sensitivity", sensitivity,
                "--min_conf", "0.8"
            ]
            
            try:
                subprocess.run(cmd, check=True)
                print(f"  BirdNET analysis completed for {site}")
            except subprocess.CalledProcessError as e:
                print(f"  Error running BirdNET for site {site}: {e}")
                continue
            
            # Find all TXT files in the output directory
            txt_files = [f for f in os.listdir(out_path) if f.endswith('.txt')]
            
            if not txt_files:
                print(f"  No TXT files found for site {site}")
                continue
            
            # Create site-specific combined file
            site_txt_path = os.path.join(out_path, f"{site}_combined.txt")
            site_header_written = False
            site_detection_count = 0
            
            with open(site_txt_path, 'w', encoding='utf-8') as site_file:
                
                # Process each TXT file for this site
                for txt_file in txt_files:
                    txt_file_path = os.path.join(out_path, txt_file)
                    
                    try:
                        with open(txt_file_path, 'r', encoding='utf-8') as f:
                            lines = f.readlines()
                            
                            if not lines:
                                continue
                            
                            # Process header
                            if not site_header_written:
                                # Add new columns to header for site file
                                header = lines[0].strip() + '\tSite\tSource_File\tDate\tTime\n'
                                site_file.write(header)
                                site_header_written = True
                                
                                # Write header to master file if not done yet
                                if not header_written:
                                    master_file.write(header)
                                    header_written = True
                            
                            # Extract date and time from the source audio filename
                            # The txt_file is typically named after the source audio file
                            source_audio_name = txt_file.replace('.BirdNET.results.txt', '')
                            date_str, time_str = extract_datetime_from_filename(source_audio_name)
                            
                            # Process data lines (skip header)
                            for line in lines[1:]:
                                if line.strip():  # Skip empty lines
                                    # Add site, source file, date, and time info to each line
                                    enhanced_line = line.strip() + f'\t{site}\t{txt_file}\t{date_str}\t{time_str}\n'
                                    site_file.write(enhanced_line)
                                    master_file.write(enhanced_line)
                                    site_detection_count += 1
                        
                        print(f"    Processed: {txt_file} (Date: {date_str}, Time: {time_str})")
                        
                    except Exception as e:
                        print(f"    Error reading {txt_file}: {e}")
            
            print(f"  Created site combined file: {site_txt_path}")
            print(f"  Total detections for {site}: {site_detection_count}")
            site_files_processed += 1

print(f"\n=== PROCESSING COMPLETE ===")
print(f"Sites processed: {site_files_processed}")
print(f"Master file created: {master_txt_path}")
print(f"\nTo create CSV files:")
print(f"1. Open {master_txt_path} in Excel")
print(f"2. Excel should automatically separate the columns (it's tab-delimited)")
print(f"3. Save As -> CSV format")
print(f"4. Do the same for individual site files in each site folder")

print(f"\nFiles created:")
print(f"- Master combined file: {master_txt_path}")
for site in os.listdir(base_audio_folder):
    site_path = os.path.join(base_audio_folder, site)
    if os.path.isdir(site_path):
        site_txt_path = os.path.join(output_base, site, f"{site}_combined.txt")
        if os.path.exists(site_txt_path):
            print(f"- Site file: {site_txt_path}")

print(f"\nNote: Date and Time columns added based on audio filename patterns.")
print(f"If some files show empty Date/Time, check the filename format and adjust the patterns in extract_datetime_from_filename() function.")