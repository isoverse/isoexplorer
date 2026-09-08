# Metadata selector table UI

The UI for a
[`ie_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_metadata_server.md)
file/analysis selector: a fullscreen-capable card with a toolbar
(select-all / deselect on the left, column / search controls on the
right) above a table that fills the rest of the card. Pair it with one
of the `*_metadata_server()` functions using the same `id`.

## Usage

``` r
ie_metadata_ui(id)
```

## Arguments

- id:

  the module id (must match the paired `*_metadata_server()`)

## Value

a [`bslib::card()`](https://rstudio.github.io/bslib/reference/card.html)
UI element

## See also

[`ie_scans_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md),
[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md)

## Examples

``` r
# pair with a *_metadata_server() on the same id inside a shiny app
ie_metadata_ui("meta")
#> <div class="container-fluid">
#>   <div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-full-screen="false" data-require-bs-caller="card()" data-require-bs-version="5" id="bslib-card-7217">
#>     <div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
#>       <div class="d-flex align-items-center justify-content-between gap-2 mb-2">
#>         <div class="d-flex gap-2">
#>           <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>             <template>Select all items that match the current search in addition to those already selected.</template>
#>             <button id="meta-metadata-select_all" type="button" class="btn btn-default action-button" style=""><span class="action-icon"><i class="far fa-square-minus" role="presentation" aria-label="square-minus icon"></i></span><span class="action-label">Select all</span></button>
#>           </bslib-tooltip>
#>           <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>             <template>Deselect all items (even those not visible in the current search)</template>
#>             <button id="meta-metadata-deselect_all" type="button" class="btn btn-default action-button" style=""><span class="action-icon"><i class="far fa-square" role="presentation" aria-label="square icon"></i></span><span class="action-label">Deselect</span></button>
#>           </bslib-tooltip>
#>         </div>
#>         <div class="d-flex gap-2">
#>           <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>             <template>Pick which columns to show</template>
#>             <button id="meta-metadata-pick_cols" type="button" class="btn btn-default action-button" style=""><span class="action-icon"><i class="fas fa-gear" role="presentation" aria-label="gear icon"></i></span><span class="action-label">Adj. View</span></button>
#>           </bslib-tooltip>
#>           <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>             <template>Toggle advanced column search option</template>
#>             <button id="meta-metadata-col_search" type="button" class="btn btn-default action-button" style=""><span class="action-icon"><i class="fas fa-magnifying-glass" role="presentation" aria-label="magnifying-glass icon"></i></span><span class="action-label">Adv. Search</span></button>
#>           </bslib-tooltip>
#>         </div>
#>       </div>
#>       <div class="shiny-spinner-output-container shiny-spinner-hideui html-fill-item html-fill-container" data-spinner-id="spinner-e01e082352b163c4c96c095bbf2b9ee3">
#>         <div class="load-container shiny-spinner-hidden load1">
#>           <div id="spinner-e01e082352b163c4c96c095bbf2b9ee3" class="loader">Loading...</div>
#>         </div>
#>         <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="meta-metadata-selection_table" style="width:100%;height:100%;"></div>
#>       </div>
#>       <div class="small text-muted mt-2">Double click analysis or file name row for exclusive selection.</div>
#>     </div>
#>     <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>       <template>Expand</template>
#>       <button aria-expanded="false" aria-label="Expand card" class="bslib-full-screen-enter badge rounded-pill"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" style="height:1em;width:1em;fill:currentColor;" aria-hidden="true" role="img"><path d="M20 5C20 4.4 19.6 4 19 4H13C12.4 4 12 3.6 12 3C12 2.4 12.4 2 13 2H21C21.6 2 22 2.4 22 3V11C22 11.6 21.6 12 21 12C20.4 12 20 11.6 20 11V5ZM4 19C4 19.6 4.4 20 5 20H11C11.6 20 12 20.4 12 21C12 21.6 11.6 22 11 22H3C2.4 22 2 21.6 2 21V13C2 12.4 2.4 12 3 12C3.6 12 4 12.4 4 13V19Z"/></svg></button>
#>     </bslib-tooltip>
#>     <script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
#>   </div>
#> </div>
```
