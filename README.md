# blockr.rtf

RTF file parsing blocks for blockr.

## Blocks

- `new_rtf_block()` - Parse RTF files to data frames (with optional pivoting)
- `new_topline_block()` - Parse specific example RTF tables to ARD format
- `new_card_block()` - Parse specific example RTF tables to CARD format

## Example

```r

# dev branches
pak::pak("BristolMyersSquibb/artful", upgrade = TRUE, ask = FALSE)
pak::pak("BristolMyersSquibb/blockr.rtf@rtf_to_df", upgrade = TRUE, ask = FALSE)

# make sure core pakcages are latest
pak::pak("BristolMyersSquibb/blockr.core", upgrade = TRUE, ask = FALSE)
pak::pak("BristolMyersSquibb/blockr.ui", upgrade = TRUE, ask = FALSE)
pak::pak("BristolMyersSquibb/blockr.dplyr", upgrade = TRUE, ask = FALSE)

library(blockr.rtf)
library(blockr.core)
library(blockr.ui)
library(blockr.dplyr)
library(blockr.topline)

options(blockr.topline_dir = "~/bms-local/topline-examples")
stopifnot(dir.exists(blockr.core::blockr_option("topline_dir", "")))

# blockr.core::serve(new_rtf_to_df_block(file = "rt-ae-ae1.rtf"))

blockr.core::serve(
  blockr.ui::new_dag_board(
    blocks = c(
      demo = new_rtf_to_df_block(file = "rt-ae-ae1.rtf"),
      demo_rename = new_rename_block(renames = list(variable = "Safety Parameter n(%)")),
      demo_mutate = new_mutate_block(string = list(
        `DEUC 6 mg  N = 332` = 'gsub("&nbsp;", " ", `DEUC 6 mg  N = 332`)')
      ),
      demo_flextable = new_flextable_conversion_block()
    ),
    links = c(
      rename_link = new_link("demo", "demo_flextable", "data"),
      mutate_link = new_link("demo_rename", "demo_mutate", "data")
      # flextable_link = new_link("demo_mutate", "demo_flextable", "data")
    )
  )
)


```