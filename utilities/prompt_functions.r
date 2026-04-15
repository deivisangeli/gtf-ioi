####################################################################################

###Function with prompts for the IMO participants

####################################################################################

###Main Prompt

###Apply prompt
prompt_fun <- function(name, country, lastYear) {
  paste0(
    name, " has participated in the International Math Olympiad for the last time in ", lastYear, ", representing ", country, ".\n",
    "You need to collect data on their career trajectory, for an academic study. Start by searching LinkedIn and other personal or institutional pages. Do NOT stop at the first profile you find or at the first failure; examine multiple sources and cross-check identities before concluding. E.g., ask if the last year of IMO participation makes sense with their current career stage, year of BA, etc. Read resumes when available.\n",
    "If the person is from a country where English is not commonly spoken, also search in the local language in addition to English.\n\n",

    "When determining `BA_institution`, APPLY this pre-Bologna heuristic:\n",
    "- If the person is European AND the year is < 1999 AND the earliest post-secondary degree found is labeled as a master's/integrated long-cycle (e.g., 'Diplom', 'Magister', 'Laurea' pre-Bologna, 'Licenciatura' pre-Bologna, 'Dipl.-Ing.', or integrated master's such as 'MEng', 'MPhys'), AND that program starts within 0–2 years of the IMO participation (or at typical undergrad entry ages 17–21), THEN TREAT THAT DEGREE’S INSTITUTION AS the `BA_institution` (equivalent to a bachelor under the later three-cycle system).\n",
    "- If a clear standalone bachelor's exists, prefer it over the heuristic above.\n",
    "- If evidence is ambiguous, choose the most plausible first-cycle equivalent based on country timelines and degree structure, but still return ONLY the JSON below.\n\n",

    "Return the result strictly as a JSON object with the following keys:\n",
    "- `profile_found`: 0 if no reliable information could be found about this person, 1 otherwise.\n",
    "- `linkedin_profile`: URL to their LinkedIn profile, if available.\n",
    "- `bio`: A concise 10-50 word description of the person's current or past professional activities. For example, \"investor, founder of company X\" or \"prominent researcher in analytic philosophy.\"\n",
    "- `wikipedia_page`: URL to the person's Wikipedia page, if any.\n",
    "- `personal_page`: URL to the person's personal or academic website, if any.\n",
    "- `recognitions`: List ONLY major prizes and fellowships. EXCLUDE student/trainee fellowships or scholarships (undergraduate/master/PhD), travel grants, conference best-paper/poster awards, honor rolls, dean's lists, other routine grants, and any IMO medals.\n",
    "- `academic`: 1 if the person followed an academic career (researcher, professor, etc.), 0 otherwise.\n",
    "- `field`: Their area of work or expertise, such as mathematics, statistics, finance, computer science, business, etc.\n",
    "- `BA_institution`: The institution where the person got a Bachelor degree (apply the pre-Bologna rule above when applicable).\n",
    "- `PhD_institution`: The institution where the person got a PhD.\n\n",
    "**Do not return anything else besides this JSON object. No explanations, apologies, or text outside the JSON.**"
  )
}


###New prompt
prompt_fun_Deivis <- function(name, country, lastYear) {
  paste0(
    name, " has participated in the International Math Olympiad for the last time in ", lastYear, ", representing ", country, ".\n",
    "You need to collect data on their career trajectory, for an academic study. Start by searching LinkedIn and other personal or institutional pages. Do NOT stop at the first profile you find or at the first failure; examine multiple sources and cross-check identities before concluding. E.g., ask if the last year of IMO participation makes sense with their current career stage, year of BA, etc. Read resumes when available.\n",
    "If the person is from a country where English is not commonly spoken, also search in the local language in addition to English.\n\n",

    "When determining `BA_institution`, APPLY this pre-Bologna heuristic:\n",
    "- If the person is European AND the year is < 1999 AND the earliest post-secondary degree found is labeled as a master's/integrated long-cycle (e.g., 'Diplom', 'Magister', 'Laurea' pre-Bologna, 'Licenciatura' pre-Bologna, 'Dipl.-Ing.', or integrated master's such as 'MEng', 'MPhys'), AND that program starts within 0–2 years of the IMO participation (or at typical undergrad entry ages 17–21), THEN TREAT THAT DEGREE’S INSTITUTION AS the `BA_institution` (equivalent to a bachelor under the later three-cycle system).\n",
    "- If a clear standalone bachelor's exists, prefer it over the heuristic above.\n",
    "- If evidence is ambiguous, choose the most plausible first-cycle equivalent based on country timelines and degree structure, but still return ONLY the JSON below.\n\n",

    "Return the result strictly as a JSON object with the following keys:\n",
    "- `profile_found`: Write 0 if no reliable information could be found about this person, 1 otherwise.\n",
    "- `linkedin_profile`: URL to their LinkedIn profile (always look for this).\n",
    "- `wikipedia_page`: URL to the person's Wikipedia page, if any.\n",
    "- `personal_page`: URL to the person's personal or academic website, if any.\n",
    "- `bio`: A concise 10-50 word description of the person's current or past professional activities. For example, \"investor, founder, or CEO of company X\" or \"prominent researcher in analytic philosophy.\"\n",
    "- `recognitions`: List major internationally recognized prizes and fellowships given for career achievement. Do not include prizes that are very local, or that have unclear prestige. Also ignore student/doctoral grants and fellowships, and any IMO medals.\n",
    "- `academic`: 1 if the person followed an academic research career, 0 otherwise.\n",
    "- `field`: Their area of work or expertise, such as mathematics, statistics, finance, computer science, business, etc.\n",
    "- `BA_institution`: The institution where the person got a Bachelor degree (apply the pre-Bologna rule above when applicable).\n",
    "- `PhD_institution`: The institution where the person got a PhD.\n\n",
    "**Do not return anything else besides this JSON object. No explanations, apologies, or text outside the JSON.**\n",
    "**If any key has no information available, leave its value empty (null or empty string), but do NOT explain or comment on the missing data.**"
  )
}

###Version with companies founded

prompt_fun_new_alt <- function(name, country, lastYear) {
  paste0(
    name, " has participated in the International Math Olympiad for the last time ", lastYear, ", representing ", country, ".\n",
    "You need to collect data on their career trajectory, for an academic study. Start by searching LinkedIn and other personal or institutional pages. Do NOT stop at the first profile you find or at the first failure; examine multiple sources and cross-check identities before concluding. E.g., ask if the last year of IMO participation makes sense with their current career stage, year of BA, etc. Read resumes when available.\n",
    "If the person is from a country where English is not commonly spoken, also search in the local language in addition to English.\n\n",

    "When determining `BA_institution`, APPLY this pre-Bologna heuristic:\n",
    "- If the person is European AND the year is < 1999 AND the earliest post-secondary degree found is labeled as a master's/integrated long-cycle (e.g., 'Diplom', 'Magister', 'Laurea' pre-Bologna, 'Licenciatura' pre-Bologna, 'Dipl.-Ing.', or integrated master's such as 'MEng', 'MPhys'), AND that program starts within 0–2 years of the IMO participation (or at typical undergrad entry ages 17–21), THEN TREAT THAT DEGREE’S INSTITUTION AS the `BA_institution` (equivalent to a bachelor under the later three-cycle system).\n",
    "- If a clear standalone bachelor's exists, prefer it over the heuristic above.\n",
    "- If evidence is ambiguous, choose the most plausible first-cycle equivalent based on country timelines and degree structure, but still return ONLY the JSON below.\n\n",

    "Return the result strictly as a JSON object with the following keys:\n",
    "- `profile_found`: Write 0 if no reliable information could be found about this person, 1 otherwise.\n",
    "- `linkedin_profile`: URL to their LinkedIn profile (always look for this).\n",
    "- `wikipedia_page`: URL to the person's Wikipedia page, if any.\n",
    "- `personal_page`: URL to the person's personal or academic website, if any.\n",
    "- `bio`: A concise 10-50 word description of the person's current or past professional activities. For example, \"investor, founder, or CEO of company X\" or \"prominent researcher in analytic philosophy.\"\n",
    "- `recognitions`: List major internationally recognized prizes and fellowships given for career achievement. Do not include prizes that are very local, or that have unclear prestige. Also ignore student/doctoral grants and fellowships, and any IMO medals.\n",
    "- `academic`: 1 if the person followed an academic research career, 0 otherwise.\n",
    "- `field`: Their area of work or expertise, such as mathematics, statistics, finance, computer science, business, etc.\n",
    "- `BA_institution`: The institution where the person got a Bachelor degree (apply the pre-Bologna rule above when applicable).\n",
    "- `PhD_institution`: The institution where the person got a PhD.\n",
    "- `founded_companies`: List the exact names of companies they founded.\n\n",
    "**Do not return anything else besides this JSON object. No explanations, apologies, or text outside the JSON.**\n",
    "**If any key has no information available, leave its value empty (null or empty string), but do NOT explain or comment on the missing data.**"
  )
}


###Bio classifier
###Classify bios into useful and not useful
prompt_useless <- function(bio_text) {
  prompt <- paste0(
    "You are given a short “bio” string. Decide whether this bio indicates that *no public information* was found. ",
    "If it does, respond **1**. Otherwise, respond **0**. ",
    "Do not add any additional words, explanation, JSON, or formatting — only the single digit.\n\n",
    "Here is the bio:\n\"",
    bio_text,
    "\""
  )
  return(prompt)
}


###Field classifier
prompt_fields <- function(bio_text, field_text) {
  prompt <- paste0(
    "You are given two strings: a “bio” and a “field” (professional / academic field). ",
    "Your task is to classify whether the person is meaningfully associated with each of these four domains: math, finance, academic, IT, ",
    "and also whether they are a founder or a CEO. Additionally, extract the names of companies they founded and the companies where they are (or were) CEO. ",
    "Return exactly a JSON object with eight keys: ",
    "\"math\", \"finance\", \"academic\", \"IT\", \"founder\", \"ceo\", \"founded_companies\", \"ceo_companies\". ",
    "For the first six keys, the value must be 1 (yes) or 0 (no). ",
    "For \"founded_companies\" and \"ceo_companies\", return a single string with company names separated by '; ' (semicolon + space). ",
    "If none are found, return an empty string for those fields. ",
    "Do not output any extra text, explanation, or arrays — only the JSON with all eight keys present.\n",
    "\n",
    "Rules and nuance:\n",
    "- **math** = 1 only if the role is explicitly a mathematics academic / researcher / professor (pure or applied math). Do not set math = 1 for engineers, data scientists, or other uses of math in non-mathematics fields.\n",
    "- **finance** = 1 only when the person is clearly in finance disciplines like investment, financial markets, macroeconomics, public finance, banking, asset management, financial economics, or roles directly about financial instruments / capital / risk. Do *not* set finance = 1 for generic business, accounting, managerial roles unless they are finance specific.\n",
    "- **IT** = 1 for roles in computer science, software engineering, programming, systems, information technology, data engineering, and related fields.\n",
    "- **academic** = 1 if the person is in an academic / research / teaching position in any domain; 0 otherwise.\n",
    "- **founder** = 1 only if the bio clearly indicates founding/co-founding/ownership (e.g., 'founder', 'co-founder', 'owner', 'co-owner', 'cofounded', 'co-founded', 'founded'). Do *not* set to 1 for 'founding engineer' or 'founding team' unless it explicitly implies company founding/ownership.\n",
    "- **ceo** = 1 only if the person is (or was) CEO (e.g., 'CEO', 'Chief Executive Officer', or 'Managing Director' when used as the top executive role). Ignore non-executive or unrelated C-titles.\n",
    "- **founded_companies**: list company names the person explicitly founded/co-founded; join multiple names with '; '. If unknown or none, return an empty string.\n",
    "- **ceo_companies**: list company names where the person is/was CEO; join multiple names with '; '. If unknown or none, return an empty string.\n",
    "- For both company lists: deduplicate, remove role words (e.g., 'CEO at '), keep only clean company names.\n",
    "- Always return all eight keys. If information is missing, use 0 for booleans and empty strings for company lists. Do not provide explanations.\n",
    "\n",
    "Here are the inputs:\n",
    "Bio: \"", bio_text, "\"\n",
    "Field: \"", field_text, "\"\n\n",
    "Output (only JSON):"
  )
  return(prompt)
}

###Prompt that clean university names
prompt_fun_uni <- function(university_raw) {
  paste0(
    "INSTRUCTION: You will receive a **single raw text string** that may contain one or more university names, optionally followed by translations in parentheses, websites, company names, or other additional information.\n\n",
    
    "Your task: **Extract ONLY** the official university name(s) present in the text — **nothing else**. This means:\n",
    "- Do **not** include translations, acronyms, or parenthetical remarks.\n",
    "- Do **not** include websites, company references, or any organization that is not a university.\n",
    "- If there is **more than one university**, return all identified names separated by a semicolon `;`, with **no extra space** before or after the semicolon.\n",
    "- Do **not** include any extra commentary, explanation, or text beyond the university name(s).\n\n",
    
    "Output Format: Only the university name(s) in plain text, exactly as they appear or in their most common official form, **with no quotes**, **no numbering**, **no extra punctuation**, and **no additional commentary**.\n\n",
    
    "Examples:\n",
    "- Input: \"Universiti Kebangsaan Malaysia (National University of Malaysia). (greateasternlife.com...)\" → Output: Universiti Kebangsaan Malaysia\n",
    "- Input: \"Universidade de São Paulo (USP) / Harvard University (harvard.edu)\" → Output: Universidade de São Paulo; Harvard University\n\n",
    
    "Raw input: ", university_raw
  )
}

prompt_fun_linkedin <- function(name, country, lastYear) {
  paste0(
    name, " participated for the last time in the International Math Olympiad in ", lastYear,
    ", representing ", country, ".\n",
    "Your task is to find the URL of this person's LinkedIn profile.\n\n",
    "Guidelines:\n",
    "- Start by searching on LinkedIn and through general web search using filters such as site:linkedin.com/in or site:linkedin.com/pub.\n",
    "- Do not stop at the first result — examine multiple sources and cross-check identities before concluding.\n",
    "- Verify if the last IMO participation year is consistent with their current career stage, BA year, or professional milestones.\n",
    "- If the person is from a non-English-speaking country, also search in the local language in addition to English.\n\n",
    "Output: return **only** the URL of the LinkedIn profile (just the link, no text around it).\n",
    "If no reliable profile is found, return exactly: NA"
  )
}

prompt_fun_gender <- function(name, country, lastYear) {
  paste0(
    name, " participated for the last time in the International Math Olympiad in ", lastYear,
    ", representing ", country, ".\n",
    "Your task is to estimate this person’s most likely gender (male, female, or unknown) based solely on the name, country, and cultural context, without performing any online searches.\n\n",
    "Guidelines:\n",
    "- Use the first name (or given name) as the main clue for gender.\n",
    "- Consider how that given name is typically gendered in the country or culture specified.\n",
    "- **Be careful: naming order (first name, family name) can vary by country** — ensure you’re identifying the correct given name in that cultural context.\n",
    "- If the name is ambiguous or lacks strong gender signals in that context, return 'unknown'.\n",
    "- Avoid speculative assumptions; if there’s insufficient evidence, prefer 'unknown'.\n\n",
    "Output: return exactly one of the following:\n",
    "- 'male'\n",
    "- 'female'\n",
    "- 'unknown'"
  )
}


prompt_fun_positions <- function(unified_title) {
  paste0(
    "You are an expert classifier of job titles.\n\n",
    "Given a single input string representing a job title in the format:\n",
    "  \"title_translated | title_raw\"\n\n",
    "(e.g. “Chief Executive Officer | CEO”, “Assistant to the CEO | CEO Assistant”, “Founder & CTO | Co-founder and Technical Head”, etc.),\n\n",
    "decide whether this title truly represents the role of **CEO** or **Founder** (or an equivalent top founding/executive leadership role).\n\n",
    "- If the title **does** refer to a “CEO/Founder” (or very close equivalent), return **0**.\n",
    "- Otherwise (e.g. “Assistant to CEO”, “Vice President”, “Team Lead”, “Team Founder”  ,etc.), return **1**.\n\n",
    "Your output should be exactly one digit (0 or 1) — nothing else (no extra text, explanation, or punctuation).\n\n",
    "Now classify this:\n`", unified_title, "`"
  )
}


prompt_fun_recognitions_clean_links <- function(recognitions_text) {
  paste0(
    "You will receive a single string from a 'recognitions' column.\n",
    "Your job is ONLY to remove web links (URLs) from the text and return the cleaned string.\n\n",
    "Removal rules:\n",
    "- Remove raw URLs like 'http://...', 'https://...', 'ftp://...'.\n",
    "- Remove 'www.' links and bare domains like 'example.org/path?x=1'.\n",
    "- Remove angle- or parentheses-wrapped URLs (e.g., '<https://...>', '(https://...)').\n",
    "- For Markdown-style links '[visible text](https://...)', REMOVE EVERYTHING (both the visible text and the URL).\n",
    "- Also remove reference-style Markdown links like '[text][ref]' and their definitions '[ref]: https://...'.\n",
    "- After removals, collapse multiple spaces into a single space and trim leading/trailing spaces.\n",
    "- Do NOT alter non-link content besides spacing cleanup.\n",
    "- Output must be EXACTLY the cleaned string (no extra commentary).\n\n",
    "Examples:\n",
    "- 'Fields Medal (2018) – IMU, details: https://imu.org/fields' → 'Fields Medal (2018) – IMU, details:'\n",
    "- 'MacArthur Fellowship – see www.macfound.org/fellows' → 'MacArthur Fellowship – see'\n",
    "- 'Rhodes Scholarship [profile](https://rhodeshouse.ox.ac.uk/scholars)' → 'Rhodes Scholarship'\n",
    "- 'Sloan Research Fellowship (<https://sloan.org/fellowships>)' → 'Sloan Research Fellowship'\n",
    "- 'Nobel Prize in Physics [press release][nobelpr]\n[nobelpr]: https://www.nobelprize.org/...' → 'Nobel Prize in Physics'\n\n",
    "Input:\n`", recognitions_text, "`"
  )
}



prompt_fun_companies <- function(name, country, lastYear, company, role) {
  paste0(
    name, " participated in the International Mathematical Olympiad, last competing in ", lastYear,
    ", representing ", country, ". They are associated with the following company in the role of '", role, "': ",
    company, ".\n\n",
    
    "Your task is to perform a careful, multi-source web search to collect reliable information about this company.\n",
    "You MUST:\n",
    "- Search across multiple independent sources (e.g., LinkedIn, Crunchbase, PitchBook, Bloomberg, official filings, business registries, press releases, reputable news outlets).\n",
    "- Cross-check information and avoid relying on a single site.\n",
    "- If the person is from a non-English-speaking country, also search in the local language.\n",
    "- DO NOT fabricate data. If a value cannot be found, return null.\n",
    "- For private companies, return market cap **only if** a credible valuation estimate is publicly available.\n",
    "- For number of employees, use the most recent reliable figure or estimate available.\n",
    "- Determine the current headquarters country.\n",
    "- If multiple companies with similar names exist, disambiguate using founder/CEO name, industry, location, or year of foundation.\n\n",
    
    "Return the result STRICTLY as a JSON object with the following keys:\n",
    "- `company`: Cleaned, canonical name of the company.\n",
    "- `hq_country`: Country of the company’s headquarters.\n",
    "- `market_cap_or_valuation`: Latest credible market capitalization (if public) or valuation estimate (if private). Express the value with currency and year of the valuation, e.g. \"USD 450 million (2024)\". Use null if unavailable.\n",
    "- `employees`: Most recent reliable estimate of number of employees. Use null if unavailable.\n",
    "- `active`: Dummy variable coded as 1 if the company is currently active, 0 if it is not active. Use null if unknown.\n",
    "- `traded`: Dummy variable coded as 1 if the company is publicly traded, 0 if it is private / not publicly traded. Use null if unknown.\n",
    "- `sources`: A list of the most important URLs used to verify the information (LinkedIn pages, Crunchbase, business registries, press releases, etc.).\n\n",
    
    "**Rules:**\n",
    "- Output ONLY the JSON object. No text before or after.\n",
    "- Do NOT add commentary, explanations, uncertainty statements, or apologies.\n",
    "- If any field has no available data, return null.\n"
  )
}







