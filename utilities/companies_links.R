################################################################################

###Top ai companies links

################################################################################


links_linkedin <- c(
  "Abridge AI" = "https://www.linkedin.com/company/abridgehq",
  "Adobe" = "https://www.linkedin.com/company/adobe",
  "Advanced Micro Devices (AMD)" = "https://www.linkedin.com/company/amd",
  "Alibaba Group" = "https://www.linkedin.com/company/chinese-alibaba-group",
  "AlphaSense" = "https://www.linkedin.com/company/alphasense",
  "Anthropic PBC" = "https://www.linkedin.com/company/anthropicresearch",
  "Baichuan AI" = NA, ###Search by name
  "Baidu" = "https://www.linkedin.com/company/baidu-inc",
  "ByteDance" = "https://www.linkedin.com/company/bytedance",
  "Cerebras Systems" = "https://www.linkedin.com/company/cerebras-systems",
  "Cognition AI" = "https://www.linkedin.com/company/cognition-ai-labs",
  "Cohere" = "https://www.linkedin.com/company/cohere-ai",
  "Databricks" = "https://www.linkedin.com/company/databricks",
  "ElevenLabs" = "https://www.linkedin.com/company/elevenlabsio",
  "Etched" = "https://www.linkedin.com/company/etched-ai",
  "G42" = "https://www.linkedin.com/company/g42ai/",
  "Glean" = "https://www.linkedin.com/company/gleanwork",
  "Google DeepMind" = "https://www.linkedin.com/company/googledeepmind",
  "Groq" = "https://www.linkedin.com/company/groq",
  "Harvey" = "https://www.linkedin.com/company/harvey-ai",
  "Hugging Face" = "https://www.linkedin.com/company/huggingface",
  "Insilico Medicine" = "https://www.linkedin.com/company/in-silico-medicine",
  "Kuaishou Technology" = "https://br.linkedin.com/company/kuaishou",
  "Lightmatter" = "https://www.linkedin.com/company/lightmatter",
  "Meta Platforms" = "https://www.linkedin.com/company/meta",
  "METR" = "https://www.linkedin.com/company/metr-evals",
  "Microsoft" = "https://www.linkedin.com/company/microsoft",
  "MiniMax" = "https://sg.linkedin.com/company/minimax-ai",
  "Mistral AI SAS" = "https://www.linkedin.com/company/mistralai",
  "ModelBest" = "https://www.linkedin.com/company/modelbest",
  "Moonshot AI" = "https://www.linkedin.com/company/moon-shot-ai",
  "Nvidia" = "https://www.linkedin.com/company/nvidia",
  "OpenAI" = "https://www.linkedin.com/company/openai",
  "Palantir Technologies" = "https://www.linkedin.com/company/palantir-technologies",
  "Perplexity" = "https://www.linkedin.com/company/perplexity-ai",
  "Physical Intelligence (PI)" = "https://www.linkedin.com/company/physical-intelligence",
  "Pinecone" = "https://www.linkedin.com/company/pinecone-io",
  "Runway" = "https://www.linkedin.com/company/runwayml",
  "Safe Superintelligence" = "https://www.linkedin.com/company/ssi-ai",
  "Sakana AI" = "https://www.linkedin.com/company/sakana-ai",
  "Salesforce" = "https://www.linkedin.com/company/salesforce",
  "Scale AI" = "https://www.linkedin.com/company/scale-ai",
  "Sierra" = "https://www.linkedin.com/company/sierra",
  "Suno" = "https://www.linkedin.com/company/sunomusic",
  "Synthesia" = "https://www.linkedin.com/company/synthesia",
  "Waymo" = "https://www.linkedin.com/company/waymo",
  "Wayve Technologies" = "https://www.linkedin.com/company/wayve-technologies",
  "World Labs" = "https://www.linkedin.com/company/world-labs",
  "x.ai" = "https://www.linkedin.com/company/x-ai",
  "Xaira Therapeutics" = "https://www.linkedin.com/company/xaira-therapeutics",
  "Facebook" = "https://www.linkedin.com/company/facebook",
  "Netflix" = "https://www.linkedin.com/company/netflix",
  "Amazon" = "https://www.linkedin.com/company/amazon",
  "Apple" =  "https://www.linkedin.com/company/apple",
  "Google" = "https://www.linkedin.com/company/google"
)

###Build a data.frame with the vector
top_ai_df <- data.frame(
  company_name = names(links_linkedin),
  linkedin_url = links_linkedin,
  stringsAsFactors = FALSE
)
rownames(top_ai_df) <- NULL
