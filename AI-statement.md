---
title: "AI Statement"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{AI Statement}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# AI Statement for Blackjack R Package Project

## Use of AI and Generative AI Tools

In compliance with University policy on responsible AI use, we openly and clearly acknowledge extensive use of generative AI tools—including OpenAI ChatGPT and GitHub Copilot—in the development of this assignment.

### How AI Was Used

During the course of this project, both myself and my group members used AI tools for the following purposes:

- **Initial code generation and scaffolding:**  
  Many of the basic functions, S3 class constructors, and control-flow templates (including functions for card creation, game setup, simulation loops, and some unit tests) were generated using ChatGPT based on our functional requirements. For example, we asked ChatGPT to “write an R function to simulate the dealing of cards in Blackjack” or “provide an S3 constructor for a card object”.  
  We then thoroughly reviewed, modified, extended, and integrated this code into our own package structure, ensuring we understood and could explain all code before including it in our submission.

- **Language and documentation refinement:**  
  AI was used to polish English language in the README, function documentation, and code comments for clarity and professionalism.

- **Debugging, error resolution, and best practices:**  
  We described encountered errors or tricky R behaviors to ChatGPT, using its suggestions to fix bugs or adapt our design.  
  Example prompt:  
  > “My testthat unit test fails with a vctrs rcrd accessor error—how should I structure my S3 method?”

- **Design discussion and alternatives:**  
  AI was consulted for weighing options between different R object systems (S3 vs S4 vs R6), file structure organization, or test coverage strategies, but all final decisions and integration were made by the team.

### How AI Was *Not* Used

- AI was not used to generate assignment answers automatically without understanding.
- AI was never used to write any part of the work without team review, adaptation, and critical editing.
- No material was copied blindly from AI outputs—every AI-assisted code section was checked, reworked, or rewritten as needed.

### Accountability and Academic Integrity

We acknowledge that a substantial portion of our codebase originated as AI-assisted drafts, but all logic, structure, and final implementations were controlled, modified, and understood by us.  
We accept full responsibility for all submitted work, and confirm it reflects our own learning and decision-making as required by University standards.

- Every AI-generated suggestion or code was only used after we understood its mechanics and appropriateness for our project.
- Our use of AI was consistent with the University’s academic integrity guidelines for responsible AI use in assessment tasks.
- All prompts and uses are listed here for transparency.

### Limitations of AI Use

- AI-generated code often required rewriting or adaptation to fit our project needs and University standards.
- Some AI-suggested solutions contained inefficiencies or did not fully align with our requirements; we learned to spot, correct, and improve these sections.
- Test coverage is currently focused on main gameplay and simulation logic; some auxiliary features could be more robust.

---

## Example Prompts Used

- "Write an S3 constructor for a card object in R."
- "How to generate a unit test for splitting hands in Blackjack using testthat?"
- "Polish the English in this README section."
- "Explain how to fix this vctrs error in an R test."
- "Suggest a yaml grouping for pkgdown user-facing and internal functions."

---

## Statement of Responsibility

We affirm that:
- All code, documentation, and design decisions were understood, curated, and finalized by us.
- AI was used as a tool for acceleration and drafting, not as a replacement for our skills or learning.
- All use of AI is fully documented here, in line with University requirements for transparency and academic integrity.
- We are prepared to discuss or demonstrate any part of our work as evidence of our understanding and engagement.
- All team members participated in using and reviewing AI-generated code and documentation, and all share responsibility for the submitted work.

*Prepared by: Weizhu Chen, Sun Yan Joanna Ma, Aneesh Agarwal 
Date: 2025-6-6*

