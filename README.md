rubbish
=======

Miscellaneous R functions, mostly [tidyverse](http://tidyverse.org/)
related.

Install
-------

**rubbish** is not on CRAN, install it from github with:

    # install.packages("devtools")
    install_github("jrnold/rubbish")

Usage
-----

<table>
<tbody>
<tr class="odd">
<td align="left"><code>%==%</code></td>
<td align="left">Equivalent to <code>(x == y) | (is.na(x) &amp; is.na(y))</code></td>
</tr>
<tr class="even">
<td align="left"><code>betwixt</code></td>
<td align="left">A more flexible <code>between</code></td>
</tr>
<tr class="odd">
<td align="left"><code>censor</code></td>
<td align="left">Censor a vector</td>
</tr>
<tr class="even">
<td align="left"><code>complete_cases</code></td>
<td align="left">Filter data frame by complete cases</td>
</tr>
<tr class="odd">
<td align="left"><code>fct_map</code></td>
<td align="left">Replace factor levels using a function</td>
</tr>
<tr class="even">
<td align="left"><code>fct_remove</code></td>
<td align="left">Remove factor levels</td>
</tr>
<tr class="odd">
<td align="left"><code>fct_replace</code></td>
<td align="left">Replace factor levels using regular expressions</td>
</tr>
<tr class="even">
<td align="left"><code>fct_seq</code></td>
<td align="left">Replace factor levels using a sequence-based pattern</td>
</tr>
<tr class="odd">
<td align="left"><code>filter_na</code></td>
<td align="left">Filter a table to remove rows with missing values</td>
</tr>
<tr class="even">
<td align="left"><code>from_dummies</code></td>
<td align="left">Create a categorical vector from multiple dummy variables.</td>
</tr>
<tr class="odd">
<td align="left"><code>glob</code></td>
<td align="left">Use globbing to select variables in <code>dplyr</code></td>
</tr>
<tr class="even">
<td align="left"><code>prop</code></td>
<td align="left">Proportion of observations by group</td>
</tr>
<tr class="odd">
<td align="left"><code>rename_map</code></td>
<td align="left">Rename columns in a <code>tbl</code> with a function</td>
</tr>
<tr class="even">
<td align="left"><code>rename_replace</code></td>
<td align="left">Rename columns in a <code>tbl</code> with a regular expression</td>
</tr>
<tr class="odd">
<td align="left"><code>rename_seq</code></td>
<td align="left">Rename columns in a <code>tbl</code> with sequential names</td>
</tr>
<tr class="even">
<td align="left"><code>rep_rows</code></td>
<td align="left">Repeat rows in a data frame</td>
</tr>
<tr class="odd">
<td align="left"><code>select_col</code></td>
<td align="left">Select a single column of a data frame and return a vector</td>
</tr>
<tr class="even">
<td align="left"><code>set_names_map</code></td>
<td align="left">Set names of a vector with a function</td>
</tr>
<tr class="odd">
<td align="left"><code>set_names_seq</code></td>
<td align="left">Set names of a vector sequentially</td>
</tr>
<tr class="even">
<td align="left"><code>to_dummies</code></td>
<td align="left">Create dummy variables from a categorical vector</td>
</tr>
<tr class="odd">
<td align="left"><code>truncare</code></td>
<td align="left">Truncate a vector</td>
</tr>
<tr class="even">
<td align="left"><code>unfill</code></td>
<td align="left">Sparsify a vector by removing repeated values</td>
</tr>
</tbody>
</table>
