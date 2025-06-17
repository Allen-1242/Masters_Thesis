from sentence_transformers import SentenceTransformer
from sklearn.preprocessing import normalize
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np
import json

class TriggerReranker:
    def __init__(self, model_name="Qwen/Qwen3-Embedding-0.6B", decision_file="enriched_decisions.jsonl"):
        self.model = SentenceTransformer(model_name)
        self.cases = [json.loads(line) for line in open(decision_file)]
        self.triggers = [case["trigger"] for case in self.cases]

        # Precompute normalized trigger embeddings
        trigger_embeddings = self.model.encode(self.triggers, convert_to_numpy=True)
        self.trigger_embeddings = normalize(trigger_embeddings, axis=1)

    def rank(self, query: str, top_k: int = 3):
        query_embedding = self.model.encode([query], convert_to_numpy=True)
        query_embedding = normalize(query_embedding, axis=1)

        # Compute cosine similarity
        sims = cosine_similarity(query_embedding, self.trigger_embeddings)[0]
        top_indices = sims.argsort()[::-1][:top_k]

        ranked_cases = []
        for idx in top_indices:
            case = self.cases[idx]
            ranked_cases.append({
                "score": float(sims[idx]),
                "trigger": case["trigger"],
                "suggestion": case["suggestion"],
                "resolution_type": case.get("resolution_type", ""),
                "filename": case.get("filename", ""),
            })

        return ranked_cases
