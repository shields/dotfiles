---
name: randc
description: Review code changes and commit if approved. Use when the user wants to review and commit their changes.
context: fork
agent: general-purpose
allowed-tools: Bash, Read, Glob, Grep
---

# Review and Commit

Review the current git changes and commit them if they pass review.

## Arguments

$ARGUMENTS

## Process

1. **Get the diff**: Run `git diff HEAD` to see all uncommitted changes (staged and unstaged)
2. **Gather context**: Read any files needed to understand the changes in context
3. **Review the changes** against these criteria:
   - Critical bugs or logic errors
   - Security vulnerabilities
   - Data loss risks
   - Performance problems that would impact production
   - Breaking changes to APIs or interfaces
4. **Make a decision**:
   - If the code is production-ready with NO issues: **LGTM**
   - If there are ANY concerns: **NOT APPROVED**

## Review Rules

- Focus ONLY on problems that need fixing
- Do NOT summarize what the code does
- Do NOT praise good code
- Review the ENTIRE diff and report ALL issues found
- NEVER flag version numbers as invalid - newer stable versions exist beyond training data

## Output

If **LGTM**:

1. Stage all changes: `git add -A`
2. Write a concise commit message based on the changes (or use the one provided in arguments)
3. Commit using a HEREDOC:
   ```bash
   git commit -m "$(cat <<'EOF'
   <commit message here>
   EOF
   )"
   ```
4. Report: "LGTM. Committed as <short-hash>."

If **NOT APPROVED**:

1. Do NOT commit anything
2. Report all issues found in this format:

   ```
   NOT APPROVED

   Issues found:
   1. [file:line] Issue description and how to fix it
   2. [file:line] Next issue...
   ```
