import faiss
import json
import numpy as np
from sentence_transformers import SentenceTransformer

# Load the FAISS index
index = faiss.read_index("calfresh_triggers.faiss")

# Load metadata (doc_id, suggestion, etc.)
with open("calfresh_metadata.json", "r") as f:
    metadata = json.load(f)

# Load the same model used for embedding
model = SentenceTransformer("all-MiniLM-L6-v2")

def search_similar_triggers(query, top_k=3):
    query_vec = model.encode([query])
    distances, indices = index.search(np.array(query_vec), top_k)

    results = []
    for i in indices[0]:
        match = metadata[str(i)]  # keys in metadata are likely stringified
        results.append({
            "matched_trigger": match["trigger"],
            "suggestion": match["suggestion"],
            "doc_id": match["doc_id"],
            "resolution_type": match["resolution_type"],
            "source": match["source"]
        })
    return results

user_query = "They cut off my benefits because they thought I changed my address"
matches = search_similar_triggers(user_query, top_k=3)

for r in matches:
    print(f" Matched Trigger: {r['matched_trigger']}")
    print(f"Suggestion: {r['suggestion']}")
    print(f"Doc ID: {r['doc_id']} | Resolution: {r['resolution_type']}\n")
