library(blockr.rtf)
library(blockr.core)
library(blockr.ui)
library(blockr.dplyr)
library(blockr.topline)

# blockr.core::serve(new_rtf_to_df_block(file = "rt-ae-ae1.rtf"))

# pak::pak("BristolMyersSquibb/blockr.core", upgrade = TRUE, ask = FALSE)
# pak::pak("BristolMyersSquibb/artful", upgrade = TRUE, ask = FALSE)
# pak::pak("BristolMyersSquibb/blockr.rtf", upgrade = TRUE, ask = FALSE)

library(dplyr)
options(blockr.topline_dir = "~/bms-local/topline-examples")
stopifnot(dir.exists(blockr.core::blockr_option("topline_dir", "")))
# blockr.core::serve(blockr.core::new_board(
#     blocks = blockr.rtf::new_card_block("rt-ae-ae1.rtf")
# ))

blockr.core::serve(
  blockr.ui::new_dag_board(
    blocks = c(
      demo_data = new_rtf_to_df_block(file = "rt-dm-demo.rtf"),
      demo_filter = new_value_filter_block(
        conditions = list(
          list(
            column = ".variable_1",
            values = c("ETHNICITY n(%)", "SEX n(%)"),
            mode = "include",
            type = "values"
          )
        )
      ),
      demo_mutate = new_mutate_block(
        string = list(
          ".bold" = ".indent == 0"
        )
      ),
      demo_flextable = new_flextable_enhanced_block(
        first_col_width = 4,
        col_colors = c("white", "gray", "blue", "orange"),
        color_full_column = TRUE
      )
    ),
    links = c(
      new_link("demo_data", "demo_filter", "data"),
      new_link("demo_filter", "demo_mutate", "data"),
      new_link("demo_mutate", "demo_flextable", "data")
    )
  )
)
