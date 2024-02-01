A Kobo checker app

## Data Checker



## Question Inspection

### Overview
This tool provides insights into the relationships surrounding a specific question in your kobo tool. Given the input question, it generates visualizations and matrices to illustrate the contextual connections.

### Input:<br>
Name of the question which should be inspected: `I_1_income_sources`

### Output:<br>
#### Parents Tree View - Shows all the questions that led to the appearance of the input question.<br>
The tree is shown in right-to-left format with the input question at the root.

#### Children Tree View - Shows all the questions which are dependent on the input question.<br>
The tree is shown in left-to-right format with the input question at the root.

#### Parents Relationship Matrix - Represent all edges from the Parents Tree View in the table format.<br>
| child | parent | formula | depth |
|----------|----------|----------|----------|
| child question name | parent question name | relevant condition | depth from the input question |

#### Children Relationship Matrix - Represent all edges from the Children Tree View in the table format.<br>
Table has the same structure as Parents Relationship Matrix.
