from shiny import App, ui, render, reactive
import pandas as pd
from io import BytesIO

# Define UI
app_ui = ui.page_fluid(
    ui.h2("Excel File Matcher"),
    ui.input_file("file1", "Upload File Containing IDs (Excel)", accept=[".xlsx"]),
    ui.input_file("file2", "Upload File to Extract Data from (Excel)", accept=[".xlsx"]),
    ui.output_text_verbatim("status"),
    ui.output_table("preview"),  # Show preview of merged data
    ui.download_button("download", "Download Output File"),
)

# Define Server
def server(input, output, session):
    @reactive.calc
    def process_data():
        file1 = input.file1()
        file2 = input.file2()

        if not file1 or not file2:
            return "Please upload both files."

        # Read uploaded Excel files
        df_ids = pd.read_excel(file1[0]["datapath"])
        df_data = pd.read_excel(file2[0]["datapath"])

        # Validate that both files contain an "ID" column
        if "ID" not in df_ids.columns or "ID" not in df_data.columns:
            return "Error: One or both files are missing an 'ID' column."

        # Perform a left merge on "ID"
        df_merged = df_ids.merge(df_data, on="ID", how="left")

        return df_merged

    @render.text
    def status():
        data = process_data()
        if isinstance(data, str):  # If an error occurred, return the error message
            return data
        return "Files successfully processed. Preview below."

    @render.table
    def preview():
        data = process_data()
        if isinstance(data, str):  # If there's an error, don't return a table
            return None
        return data.head(10)  # Show first 10 rows of the merged output

    @session.download(filename="output.xlsx")
    def download():
        df = process_data()
        if isinstance(df, str):  # If an error occurred, stop download
            return None

        output_buffer = BytesIO()

        # Use ExcelWriter to properly write the DataFrame
        with pd.ExcelWriter(output_buffer, engine="openpyxl") as writer:
            df.to_excel(writer, index=False)

        output_buffer.seek(0)  # Move to start of buffer

        # Yield the file content as a stream (prevents file corruption)
        yield output_buffer.getvalue()



# Run the app
app = App(app_ui, server)
