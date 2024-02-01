A Kobo checker app

## Data Checker

This app allows to check the user's Kobo file for compatibility with the utilityR cleaning procedures. The resulting table will have the following structure

| file | rownames | column | value | issue | cyrillic_char |priority|
|------|----------|--------|-------|-------|---------------|--------|
| Is the issue within `tool.survey` or `tool.choices` | The row of the issue | The column of the issue | The value that causes the issue |What the issue is|If the issue is the presence of cyrillic characters, they will be mentioned here| What is the priority of the issue|

All of the conflicts that will clash with the workflows of utilityR and can potentially break it will be marked as `First priority`, those that are problematic but won't directly break anything will be marked as `Second priority`.

The app will implement the following checks of your Kobo file:
- Checks for non-English charactes (`Second priority` issue)
- Checks for double/leading/trailing spaces (`Second priority` issue)
- Checks for whether a question has maximum 1 `other` subquestion (`First priority` issue)
- Checks for whether an `other` question only has 1 relevancy (`First priority` issue)
- Checks for whether a choice that triggers the `other` question is called `other` (`First priority` issue)
- Checks for whether text `other` column question name ends with `_other` (`First priority` issue)
- Checks whether relevance and constraint values for each variable are present in that variable's choice list (`First priority` issue)
- Checks for duplicate choices in the individual `list_name` (`First priority` issue)
- If the `list_name` for a given question contains `none` option, checks if `none` is present within the constraint for the question (`Second priority` issue)
- Checks if the choice `name` and `label` columns are close semantically (`Second priority` issue)


## Question Inspection

### Overview
This tool provides insights into the relationships surrounding a specific question in your kobo tool. Given the input question, it generates visualizations and matrices to illustrate the contextual connections.

### Input:<br>
Name of the question which should be inspected: `I_1_income_sources`

### Output:<br>
#### Parents Tree View
Shows all the questions that led to the appearance of the input question.<br>
The tree is shown in right-to-left format with the input question at the root.

#### Children Tree View
Shows all the questions which are dependent on the input question.<br>
The tree is shown in left-to-right format with the input question at the root.

#### Parents Relationship Matrix
Represent all edges from the Parents Tree View in the table format.<br>
| child | parent | formula | depth |
|----------|----------|----------|----------|
| child question name | parent question name | relevant condition | depth from the input question |

#### Children Relationship Matrix
Represent all edges from the Children Tree View in the table format.<br>
Table has the same structure as Parents Relationship Matrix.
