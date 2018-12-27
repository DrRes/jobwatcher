<!-- README.md is generated from README.Rmd. Please edit that file -->
jobwatcher
==========

Human Genome Center(HGC)スパコンでのパイプライン構築用Rパッケージです.  *tidyverse*, *drake*パッケージをベースにしているため, これらのパッケージをインストールしていない場合,  インストールに時間がかかる場合があります.

Install
-------

インストールには, 次のコマンドを実行してください.

``` r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("drres/jobwatcher")
library(jobwatcher)
```

Build a pipeline
----------------

次のコマンドを実行すると, そのまま実行できる形でパイプラインの雛形が作成されます.  適宜編集してお使いください.

``` r
build_pipeline("your_pipeline_name", "~/your_pipeline_path")
#> ✔ Directory '~/your_pipeline_path/log' has been created.
#> ✔ Directory '~/your_pipeline_path/script' has been created.
#> ✔ Directory '~/your_pipeline_path/config' has been created.
#> ✔ File '~/your_pipeline_path/your_pipeline_name.sh' has been written.
#> ✔ File '~/your_pipeline_path/your_pipeline_name.R' has been written.
#> ● Please edit '~/your_pipeline_path/your_pipeline_name.R' for your own pipeline.
#> ● Then, run bash ~/your_pipeline_path/your_pipeline_name.sh
```

パイプライン作成部分は完全に*drake*パッケージベースで作成しています.   パイプラインの各`qsub`操作を**R**の関数でラップしたものを組み合わせて構築する流れになります.  各関数の引数に`...`をとっていますが, ここにパイプラインの他の操作を記入することで,  記入された操作がすべて完了してからその操作が実行されるようになります.

Issues
------

このパッケージは個人的に開発したものを公開しています.  Pull request, issueは歓迎いたします.
