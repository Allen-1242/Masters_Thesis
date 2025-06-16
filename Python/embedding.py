import json
import faiss
import numpy as np
from sentence_transformers import SentenceTransformer

# === Load the JSONL ===
def load_jsonl(path):
    documents = []
    with open(path, "r") as f:
        for i, line in enumerate(f, start=1):
            try:
                data = json.loads(line.strip())

                # If it's a list of docs on one line, unpack it
                if isinstance(data, list):
                    documents.extend(data)
                elif isinstance(data, dict):
                    documents.append(data)
                else:
                    print(f"⚠️ Line {i} has unexpected type: {type(data)}")

            except json.JSONDecodeError as e:
                print(f" JSON decode error on line {i}: {e}")
    return documents

documents = list(load_jsonl("recourse_dataset.jsonl"))

# === Prepare trigger texts ===
texts = [doc["trigger"] for doc in documents]

# === Generate embeddings ===
model = SentenceTransformer("all-MiniLM-L6-v2")
embeddings = model.encode(texts, convert_to_numpy=True)

# === Build FAISS index ===
dimension = embeddings.shape[1]
index = faiss.IndexFlatL2(dimension)
index.add(embeddings)

# === Create metadata mapping ===
metadata = {
    i: {
        "doc_id": doc["doc_id"],
        "source": doc["source"],
        "trigger": doc["trigger"],
        "suggestion": doc["suggestion"],
        "resolution_type": doc["resolution_type"],
        "filename": doc["filename"]
    }
    for i, doc in enumerate(documents)
}

# === Save index and metadata ===
faiss.write_index(index, "calfresh_triggers.faiss")

with open("calfresh_metadata.json", "w") as f:
    json.dump(metadata, f)

print(f"Embedded {len(texts)} triggers and built FAISS index.")
