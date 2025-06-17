import pdfplumber
import re
import json

# === Step 1: Extract raw text from the PDF ===
pdf_path = "fsman04a.pdf"  # adjust path as needed
text = ""

with pdfplumber.open(pdf_path) as pdf:
    for page in pdf.pages:
        text += page.extract_text() + "\n"

# === Step 2: Parse the text into structured entries ===
lines = text.splitlines()
data = []

main_section = ""
subsection_code = ""
subsection_title = ""

for line in lines:
    line = line.strip()

    # Match main section titles (e.g., "ELIGIBILITY STANDARDS")
    if re.match(r'^[A-Z\s]{5,}$', line) and "TABLE OF CONTENTS" not in line:
        main_section = line.title()
        continue

    # Match subsection mapping (e.g., "Residency ..................... 63-401")
    match = re.match(r'^(.*?)\.+\s+63-(\d+)', line)
    if match:
        subsection_title = match.group(1).strip().title()
        subsection_code = match.group(2)
        continue

    # Match actual legal clauses (e.g., ".1 A household must be...")
    clause_match = re.match(r'^\.(\d+)\s+(.*)', line)
    if clause_match and subsection_code:
        citation = f"MPP §63-{subsection_code}"
        clause_text = clause_match.group(2).strip()
        embedding_input = f"{main_section}: {subsection_title} - {clause_text}"

        data.append({
            "citation": citation,
            "main_section": main_section,
            "subsection": subsection_title,
            "text": clause_text,
            "embedding_input": embedding_input
        })

# === Step 3: Write to a .jsonl file ===
output_path = "mpp_legal_clauses.jsonl"

with open(output_path, "w") as f:
    for entry in data:
        f.write(json.dumps(entry) + "\n")

print(f"Exported {len(data)} entries to {output_path}")
