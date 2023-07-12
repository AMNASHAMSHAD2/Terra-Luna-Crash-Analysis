import csv
from datetime import datetime

# Open the input file for reading
with open('wluna_price_data.csv', 'r') as input_file:
    # Open the output file for writing
    with open('wluna_price_data_time.csv', 'w', newline='') as output_file:
        # Create a CSV reader object
        reader = csv.reader(input_file)
        # Create a CSV writer object
        writer = csv.writer(output_file)
        # Write the header row to the output file
        writer.writerow(next(reader))
        # Loop through the rows of the input file
        for row in reader:
            # Check if the condition is satisfied
            
            row[0]=datetime.fromtimestamp(int(row[0])).strftime('%Y-%m-%d %H:%M:%S')

                # Write the row to the output file
            writer.writerow(row)