import pdfplumber
import ollama
import re
import json
import datetime as dt
from datetime import datetime
from pathlib import Path


def merge_json_blocks(raw: str) -> dict:

    if isinstance(raw, dict):
        return raw  # already cleaned

    raw = raw.replace("```json", "").replace("```", "").strip()
    json_blocks = re.findall(r'\{.*?\}', raw, re.DOTALL)

    merged = {}
    for block in json_blocks:
        try:
            obj = json.loads(block)
            for key, value in obj.items():
                if key not in merged or not merged[key]:
                    merged[key] = value
        except Exception:
            continue
    return merged
    """
    Extract multiple JSON blocks from LLM output and merge into a single JSON object.
    Keeps the first non-empty value per key.
    """

    # Remove markdown fences
    raw = raw.replace("```json", "").replace("```", "").strip()

    # Find all JSON blocks
    json_blocks = re.findall(r'\{.*?\}', raw, re.DOTALL)

    merged = {}

    for block in json_blocks:
        try:
            obj = json.loads(block)
            for key, value in obj.items():
                # Keep the first non-empty, non-duplicate value
                if key not in merged or not merged[key].strip():
                    merged[key] = value
        except Exception:
            continue  # Skip malformed

    return merged


def extract_key_sections_from_pdf(pdf_path, max_chars=3000):
    import pdfplumber

    section_headers = ["SUMMARY", "FACTS", "ORDER", "CONCLUSION"]
    all_possible_headers = section_headers + ["LAW", "LEGAL BASIS", "DISCUSSION", "FINDINGS", "EVIDENCE"]

    sections = {h: [] for h in section_headers}
    current_section = None

    with pdfplumber.open(pdf_path) as pdf:
        lines = []
        for page in pdf.pages:
            text = page.extract_text() or ""
            lines.extend(text.splitlines())

    i = 0
    while i < len(lines):
        line = lines[i].strip().rstrip(":.")
        
        if line in all_possible_headers:
            if line in section_headers:
                current_section = line
                i += 1
                section_lines = []

                # Collect lines until next section header
                while i < len(lines):
                    next_line = lines[i].strip()
                    next_line_clean = next_line.rstrip(":.")
                    if next_line_clean in all_possible_headers:
                        break
                    section_lines.append(next_line)
                    i += 1

                sections[current_section] = section_lines
            else:
                # Skip irrelevant sections like LAW, etc.
                current_section = None
                i += 1
                while i < len(lines):
                    next_line = lines[i].strip().rstrip(":.")
                    if next_line in section_headers:
                        break
                    i += 1
        else:
            i += 1

    # Combine collected sections
    final_blocks = []
    for h in section_headers:
        print(section_headers)
        content = "\n".join(sections[h]).strip()
        if content:
            final_blocks.append(f"{h}\n{content}")

    combined_text = "\n\n".join(final_blocks)

    print(combined_text)

    return combined_text





def generate_prompt(text):
    """Create a structured prompt to extract legal fields using LLM."""
    return f"""
You are a legal assistant AI.

Extract the following fields from the administrative hearing text below:

1. trigger: why was the CalFresh benefit denied or discontinued?
2. suggestion: what should the claimant do to resolve the issue?
3. legal_citation: any policy section cited (e.g., MPP §22-073.37)
4. resolution_type: whether the case was resolved by stipulation or judge order
5. source_case: the case number

Only return a JSON object with the fields: trigger, suggestion, legal_citation, resolution_type, and source_case.

Hearing Text:
\"\"\"
{text}
\"\"\"
"""

def extract_structured_data_from_pdf(pdf_path, model='phi3:3.8b'):
    """End-to-end pipeline: PDF -> plain text -> structured JSON via Ollama."""
    text = extract_key_sections_from_pdf(pdf_path)
    prompt = generate_prompt(text)

    response = ollama.chat(model=model, messages=[{"role": "user", "content": prompt}])
    raw = response['message']['content']
    print("LLM OUTPUT (raw):", raw[:300])  #debug

    # Clean and merge multiple JSONs into one flat dict
    structured = merge_json_blocks(raw)

    print("CLEANED STRUCTURED:", structured)
    return structured




def batch_process_directory(pdf_dir, output_path, model='phi3:3.8b'):
    """Process all PDFs in a directory and write structured data to JSONL."""
    
    STANDARD_FIELDS = {
    "doc_id": "",
    "filename": "",
    "trigger": "",
    "suggestion": "",
    "legal_citation": "",
    "resolution_type": "",
    "source_case": "",
    "source": "CalFresh_PublicDecision"
}
    output_lines = []
    pdf_dir = Path(pdf_dir)
    pdf_files = list(pdf_dir.glob("*.pdf"))

    for pdf_file in pdf_files:
        try:
            print(f"PDF starting the process: {pdf_file.name}")
            structured = extract_structured_data_from_pdf(str(pdf_file), model=model)
            structured["filename"] = pdf_file.name

            match = re.match(r"Decision_(\d+)_([0-9]{8})", pdf_file.name)
            
            if match:
                case_id = match.group(1)
                date_str = match.group(2)
                formatted_date = datetime.strptime(date_str, "%m%d%Y").strftime("%Y-%m-%d")
                doc_id = f"Decision_{case_id}_{formatted_date}"
            else:
                doc_id = f"UnknownID_{pdf_file.stem}"

            structured["doc_id"] = doc_id
            # === Enforce standardized keys and order ===
            standardized = {k: structured.get(k, v) for k, v in STANDARD_FIELDS.items()}

            output_lines.append(json.dumps(standardized))

            print(f"Processed: {pdf_file.name}")
        except Exception as e:
            print(f"Error processing {pdf_file.name}: {e}")

    # Write output to a .jsonl file
    with open(output_path, "w") as f:
        for line in output_lines:
            f.write(line + "\n")

    print(f" Finished processing {len(output_lines)} PDFs → {output_path}")

if __name__ == "__main__":
    batch_process_directory(
        pdf_dir=Path.expanduser(Path("~/Masters_Thesis/Legal_Documents")),
        output_path="recourse_dataset.jsonl",
        model="phi3:3.8b"
    )