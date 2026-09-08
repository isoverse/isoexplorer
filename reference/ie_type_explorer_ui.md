# File-selector + plot layout for one measurement type

Convenience UI combining a left file-selector sidebar (a
[`ie_metadata_ui()`](https://isoexplorer.isoverse.org/reference/ie_metadata_ui.md))
with a plot to its right – the building block each focused explorer and
each tab of
[`ie_create_isofiles_server()`](https://isoexplorer.isoverse.org/reference/ie_create_isofiles_server.md)
uses. Wire the matching `*_metadata_server()` (on `meta_id`) and
`*_plot_server()` in your server function.

## Usage

``` r
ie_type_explorer_ui(meta_id, plot_ui)
```

## Arguments

- meta_id:

  the id for the
  [`ie_metadata_ui()`](https://isoexplorer.isoverse.org/reference/ie_metadata_ui.md)
  / `*_metadata_server()` pair

- plot_ui:

  a plot module UI element, e.g. `ie_scans_plot_ui("scan")`

## Value

a
[`bslib::layout_sidebar()`](https://rstudio.github.io/bslib/reference/sidebar.html)
UI element

## See also

[`ie_file_server()`](https://isoexplorer.isoverse.org/reference/ie_file_server.md),
[`ie_scans_plot_ui()`](https://isoexplorer.isoverse.org/reference/ie_scans_plot.md),
[`ie_scans_metadata_server()`](https://isoexplorer.isoverse.org/reference/ie_typed_metadata_servers.md)

## Examples

``` r
# a file-selector sidebar next to a scans plot (place inside a shiny UI)
ie_type_explorer_ui("meta", ie_scans_plot_ui("scan"))
#> <div class="container-fluid">
#>   <div class="bslib-sidebar-layout bslib-mb-spacing html-fill-item" data-bslib-sidebar-init="TRUE" data-collapsible-desktop="true" data-collapsible-mobile="true" data-open-desktop="open" data-open-mobile="open" data-require-bs-caller="layout_sidebar()" data-require-bs-version="5" style="--_sidebar-width:40%;">
#>     <div class="main bslib-gap-spacing html-fill-container">
#>       <div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-full-screen="false" data-require-bs-caller="card()" data-require-bs-version="5" id="bslib-card-8813" style="min-height:400px;">
#>         <div class="bslib-sidebar-layout bslib-mb-spacing sidebar-right html-fill-item" data-bslib-sidebar-init="TRUE" data-collapsible-desktop="true" data-collapsible-mobile="true" data-open-desktop="open" data-open-mobile="closed" data-require-bs-caller="layout_sidebar()" data-require-bs-version="5" style="--_sidebar-width:190px;">
#>           <div class="main bslib-gap-spacing html-fill-container">
#>             <div class="d-flex align-items-center gap-2 mb-2">
#>               <div class="d-flex flex-wrap align-items-center gap-2" style="flex: 1 1 0;">
#>                 <bslib-popover placement="auto" bsOptions="{&quot;trigger&quot;:&quot;focus&quot;}" data-require-bs-version="5" data-require-bs-caller="popover()">
#>                   <template>
#>                     <div style="display:contents;">
#>                       <div style="width: 5rem;">
#>                         <div id="scan-units" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="scan-units-label">
#>                           <label class="control-label shiny-label-null" for="scan-units" id="scan-units-label"></label>
#>                           <div class="shiny-options-group">
#>                             <div class="radio">
#>                               <label>
#>                                 <input type="radio" name="scan-units" value="mV" checked="checked"/>
#>                                 <span>mV</span>
#>                               </label>
#>                             </div>
#>                             <div class="radio">
#>                               <label>
#>                                 <input type="radio" name="scan-units" value="V"/>
#>                                 <span>V</span>
#>                               </label>
#>                             </div>
#>                             <div class="radio">
#>                               <label>
#>                                 <input type="radio" name="scan-units" value="fA"/>
#>                                 <span>fA</span>
#>                               </label>
#>                             </div>
#>                             <div class="radio">
#>                               <label>
#>                                 <input type="radio" name="scan-units" value="pA"/>
#>                                 <span>pA</span>
#>                               </label>
#>                             </div>
#>                             <div class="radio">
#>                               <label>
#>                                 <input type="radio" name="scan-units" value="nA"/>
#>                                 <span>nA</span>
#>                               </label>
#>                             </div>
#>                             <div class="radio">
#>                               <label>
#>                                 <input type="radio" name="scan-units" value="µA"/>
#>                                 <span>µA</span>
#>                               </label>
#>                             </div>
#>                             <div class="radio">
#>                               <label>
#>                                 <input type="radio" name="scan-units" value="mA"/>
#>                                 <span>mA</span>
#>                               </label>
#>                             </div>
#>                             <div class="radio">
#>                               <label>
#>                                 <input type="radio" name="scan-units" value="A"/>
#>                                 <span>A</span>
#>                               </label>
#>                             </div>
#>                             <div class="radio">
#>                               <label>
#>                                 <input type="radio" name="scan-units" value="cps"/>
#>                                 <span>cps</span>
#>                               </label>
#>                             </div>
#>                           </div>
#>                         </div>
#>                       </div>
#>                     </div>
#>                     <div style="display:contents;">Units</div>
#>                   </template>
#>                   <button id="scan-units-trigger" type="button" class="btn btn-default action-button"><span class="action-icon"><i class="fas fa-caret-down" role="presentation" aria-label="caret-down icon"></i></span><span class="action-label"><span id="scan-units_label" class="shiny-text-output"></span></span></button>
#>                 </bslib-popover>
#>                 <bslib-popover id="scan-ratios_popover" placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="popover()">
#>                   <template>
#>                     <div style="display:contents;">
#>                       <div style="width: 12rem;">
#>                         <div class="form-group shiny-input-container">
#>                           <div class="checkbox">
#>                             <label>
#>                               <input id="scan-ratios_calculate" type="checkbox" class="shiny-input-checkbox"/>
#>                               <span>Calculate ratios</span>
#>                             </label>
#>                           </div>
#>                         </div>
#>                         <div class="shiny-panel-conditional" data-display-if="input.ratios_calculate == true" data-ns-prefix="scan-">
#>                           <div id="scan-ratios_params" class="shiny-html-output"></div>
#>                           <div class="form-group shiny-input-container">
#>                             <div class="checkbox">
#>                               <label>
#>                                 <input id="scan-ratios_normalize" type="checkbox" class="shiny-input-checkbox"/>
#>                                 <span>Normalize</span>
#>                               </label>
#>                             </div>
#>                           </div>
#>                         </div>
#>                         <div class="d-flex gap-1 mt-2">
#>                           <button class="btn btn-default action-button btn-sm btn-success flex-fill" id="scan-ratios_apply" type="button"><span class="action-label">Apply</span></button>
#>                           <button class="btn btn-default action-button btn-sm btn-secondary flex-fill" id="scan-ratios_cancel" type="button"><span class="action-label">Cancel</span></button>
#>                         </div>
#>                       </div>
#>                     </div>
#>                     <div style="display:contents;">Ratios</div>
#>                   </template>
#>                   <button id="scan-ratios-trigger" type="button" class="btn btn-default action-button"><span class="action-icon"><i class="fas fa-caret-down" role="presentation" aria-label="caret-down icon"></i></span><span class="action-label">Ratios</span></button>
#>                 </bslib-popover>
#>                 <bslib-popover placement="auto" bsOptions="{&quot;trigger&quot;:&quot;focus&quot;}" data-require-bs-version="5" data-require-bs-caller="popover()">
#>                   <template>
#>                     <div style="display:contents;">
#>                       <div style="width: 10rem;">
#>                         <div id="scan-scan_type_input" class="shiny-html-output"></div>
#>                       </div>
#>                     </div>
#>                     <div style="display:contents;">Scan type</div>
#>                   </template>
#>                   <button id="scan-scan_type-trigger" type="button" class="btn btn-default action-button"><span class="action-icon"><i class="fas fa-caret-down" role="presentation" aria-label="caret-down icon"></i></span><span class="action-label"><span id="scan-scan_type_label" class="shiny-text-output"></span></span></button>
#>                 </bslib-popover>
#>                 <div id="scan-species_buttons" class="shiny-html-output"></div>
#>               </div>
#>               <div class="d-flex gap-1">
#>                 <bslib-tooltip placement="auto" bsOptions="[]" class="shinyjs-disabled" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>                   <template>Show all data</template>
#>                   <button id="scan-zoom_all" type="button" class="btn btn-default action-button"><span class="action-icon"><i aria-label="resize-full icon" class="glyphicon glyphicon-resize-full" role="presentation"></i></span><span class="action-label"></span></button>
#>                 </bslib-tooltip>
#>                 <bslib-tooltip placement="auto" bsOptions="[]" class="shinyjs-disabled" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>                   <template>Move left</template>
#>                   <button id="scan-zoom_move_left" type="button" class="btn btn-default action-button"><span class="action-icon"><i class="fas fa-arrow-left" role="presentation" aria-label="arrow-left icon"></i></span><span class="action-label"></span></button>
#>                 </bslib-tooltip>
#>                 <bslib-tooltip placement="auto" bsOptions="[]" class="shinyjs-disabled" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>                   <template>Move right</template>
#>                   <button id="scan-zoom_move_right" type="button" class="btn btn-default action-button"><span class="action-icon"><i class="fas fa-arrow-right" role="presentation" aria-label="arrow-right icon"></i></span><span class="action-label"></span></button>
#>                 </bslib-tooltip>
#>                 <bslib-tooltip placement="auto" bsOptions="[]" class="shinyjs-disabled" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>                   <template>Revert to previous view</template>
#>                   <button id="scan-zoom_back" type="button" class="btn btn-default action-button"><span class="action-icon"><i class="fas fa-rotate-left" role="presentation" aria-label="rotate-left icon" verify_fa="FALSE"></i></span><span class="action-label"></span></button>
#>                 </bslib-tooltip>
#>               </div>
#>               <div class="d-flex justify-content-end" style="flex: 1 1 0;">
#>                 <bslib-tooltip placement="auto" bsOptions="[]" class="shinyjs-disabled" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>                   <template>Save the plot as a PDF</template>
#>                   <button id="scan-plot_download-download_dialog" type="button" class="btn btn-default action-button"><span class="action-icon"><i class="far fa-file-pdf" role="presentation" aria-label="file-pdf icon"></i></span></button>
#>                 </bslib-tooltip>
#>               </div>
#>             </div>
#>             <div class="shiny-spinner-output-container shiny-spinner-hideui html-fill-item html-fill-container" data-spinner-id="spinner-6a459fe8af6465b58f530f5b7a73eb0a">
#>               <div class="load-container shiny-spinner-hidden load1">
#>                 <div id="spinner-6a459fe8af6465b58f530f5b7a73eb0a" class="loader">Loading...</div>
#>               </div>
#>               <div class="shiny-plot-output html-fill-item" data-brush-clip="TRUE" data-brush-delay="300" data-brush-delay-type="debounce" data-brush-direction="x" data-brush-fill="#9cf" data-brush-id="scan-data_plot_brush" data-brush-opacity="0.25" data-brush-reset-on-new="TRUE" data-brush-stroke="#036" data-dblclick-clip="TRUE" data-dblclick-id="scan-data_plot_dblclick" id="scan-data_plot" style="width:100%;height:400px;"></div>
#>             </div>
#>           </div>
#>           <aside id="bslib-sidebar-9271" class="sidebar" hidden data-resizable>
#>             <div class="sidebar-content bslib-gap-spacing">
#>               <header class="sidebar-title">Plot Options</header>
#>               <div id="scan-aes_options" class="shiny-html-output"></div>
#>               <div class="form-group shiny-input-container">
#>                 <label class="control-label" id="scan-scales-label" for="scan-scales">Scales:</label>
#>                 <div>
#>                   <select id="scan-scales" class="shiny-input-select"><option value="free" selected>free</option>
#> <option value="fixed">fixed</option>
#> <option value="free_x">free_x</option>
#> <option value="free_y">free_y</option></select>
#>                   <script type="application/json" data-for="scan-scales" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#>                 </div>
#>               </div>
#>               <div class="form-group shiny-input-container">
#>                 <div class="checkbox">
#>                   <label>
#>                     <input id="scan-scientific" type="checkbox" class="shiny-input-checkbox"/>
#>                     <span>Scientific notation</span>
#>                   </label>
#>                 </div>
#>               </div>
#>               <div class="form-group shiny-input-container">
#>                 <div class="checkbox">
#>                   <label>
#>                     <input id="scan-drop_unused_levels" type="checkbox" class="shiny-input-checkbox"/>
#>                     <span>Drop unused levels</span>
#>                   </label>
#>                 </div>
#>               </div>
#>               <div class="form-group shiny-input-container">
#>                 <label class="control-label" id="scan-legend_position-label" for="scan-legend_position">Legend:</label>
#>                 <div>
#>                   <select id="scan-legend_position" class="shiny-input-select"><option value="right" selected>right</option>
#> <option value="bottom">bottom</option>
#> <option value="top">top</option>
#> <option value="left">left</option>
#> <option value="hide">hide</option></select>
#>                   <script type="application/json" data-for="scan-legend_position" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#>                 </div>
#>               </div>
#>               <div class="form-group shiny-input-container">
#>                 <label class="control-label" id="scan-font_size-label" for="scan-font_size">Font size:</label>
#>                 <input id="scan-font_size" type="number" class="shiny-input-number form-control" value="16" data-update-on="change" min="6" step="1"/>
#>               </div>
#>             </div>
#>           </aside>
#>           <button class="collapse-toggle" type="button" title="Toggle sidebar" aria-expanded="true" aria-controls="bslib-sidebar-9271"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-chevron-left collapse-icon" style="fill:currentColor;" aria-hidden="true" role="img" ><path fill-rule="evenodd" d="M11.354 1.646a.5.5 0 0 1 0 .708L5.707 8l5.647 5.646a.5.5 0 0 1-.708.708l-6-6a.5.5 0 0 1 0-.708l6-6a.5.5 0 0 1 .708 0z"></path></svg></button>
#>           <script data-bslib-sidebar-init>bslib.Sidebar.initCollapsibleAll()</script>
#>         </div>
#>         <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>           <template>Expand</template>
#>           <button aria-expanded="false" aria-label="Expand card" class="bslib-full-screen-enter badge rounded-pill"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" style="height:1em;width:1em;fill:currentColor;" aria-hidden="true" role="img"><path d="M20 5C20 4.4 19.6 4 19 4H13C12.4 4 12 3.6 12 3C12 2.4 12.4 2 13 2H21C21.6 2 22 2.4 22 3V11C22 11.6 21.6 12 21 12C20.4 12 20 11.6 20 11V5ZM4 19C4 19.6 4.4 20 5 20H11C11.6 20 12 20.4 12 21C12 21.6 11.6 22 11 22H3C2.4 22 2 21.6 2 21V13C2 12.4 2.4 12 3 12C3.6 12 4 12.4 4 13V19Z"/></svg></button>
#>         </bslib-tooltip>
#>         <script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
#>       </div>
#>     </div>
#>     <aside class="sidebar html-fill-container" data-resizable id="bslib-sidebar-6288">
#>       <div class="sidebar-content bslib-gap-spacing html-fill-item html-fill-container">
#>         <div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-full-screen="false" data-require-bs-caller="card()" data-require-bs-version="5" id="bslib-card-2550">
#>           <div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
#>             <div class="d-flex align-items-center justify-content-between gap-2 mb-2">
#>               <div class="d-flex gap-2">
#>                 <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>                   <template>Select all items that match the current search in addition to those already selected.</template>
#>                   <button id="meta-metadata-select_all" type="button" class="btn btn-default action-button" style=""><span class="action-icon"><i class="far fa-square-minus" role="presentation" aria-label="square-minus icon"></i></span><span class="action-label">Select all</span></button>
#>                 </bslib-tooltip>
#>                 <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>                   <template>Deselect all items (even those not visible in the current search)</template>
#>                   <button id="meta-metadata-deselect_all" type="button" class="btn btn-default action-button" style=""><span class="action-icon"><i class="far fa-square" role="presentation" aria-label="square icon"></i></span><span class="action-label">Deselect</span></button>
#>                 </bslib-tooltip>
#>               </div>
#>               <div class="d-flex gap-2">
#>                 <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>                   <template>Pick which columns to show</template>
#>                   <button id="meta-metadata-pick_cols" type="button" class="btn btn-default action-button" style=""><span class="action-icon"><i class="fas fa-gear" role="presentation" aria-label="gear icon"></i></span><span class="action-label">Adj. View</span></button>
#>                 </bslib-tooltip>
#>                 <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>                   <template>Toggle advanced column search option</template>
#>                   <button id="meta-metadata-col_search" type="button" class="btn btn-default action-button" style=""><span class="action-icon"><i class="fas fa-magnifying-glass" role="presentation" aria-label="magnifying-glass icon"></i></span><span class="action-label">Adv. Search</span></button>
#>                 </bslib-tooltip>
#>               </div>
#>             </div>
#>             <div class="shiny-spinner-output-container shiny-spinner-hideui html-fill-item html-fill-container" data-spinner-id="spinner-e01e082352b163c4c96c095bbf2b9ee3">
#>               <div class="load-container shiny-spinner-hidden load1">
#>                 <div id="spinner-e01e082352b163c4c96c095bbf2b9ee3" class="loader">Loading...</div>
#>               </div>
#>               <div class="datatables html-widget html-widget-output shiny-report-size html-fill-item" id="meta-metadata-selection_table" style="width:100%;height:100%;"></div>
#>             </div>
#>             <div class="small text-muted mt-2">Double click analysis or file name row for exclusive selection.</div>
#>           </div>
#>           <bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
#>             <template>Expand</template>
#>             <button aria-expanded="false" aria-label="Expand card" class="bslib-full-screen-enter badge rounded-pill"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" style="height:1em;width:1em;fill:currentColor;" aria-hidden="true" role="img"><path d="M20 5C20 4.4 19.6 4 19 4H13C12.4 4 12 3.6 12 3C12 2.4 12.4 2 13 2H21C21.6 2 22 2.4 22 3V11C22 11.6 21.6 12 21 12C20.4 12 20 11.6 20 11V5ZM4 19C4 19.6 4.4 20 5 20H11C11.6 20 12 20.4 12 21C12 21.6 11.6 22 11 22H3C2.4 22 2 21.6 2 21V13C2 12.4 2.4 12 3 12C3.6 12 4 12.4 4 13V19Z"/></svg></button>
#>           </bslib-tooltip>
#>           <script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
#>         </div>
#>       </div>
#>     </aside>
#>     <button class="collapse-toggle" type="button" title="Toggle sidebar" aria-expanded="true" aria-controls="bslib-sidebar-6288"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-chevron-left collapse-icon" style="fill:currentColor;" aria-hidden="true" role="img" ><path fill-rule="evenodd" d="M11.354 1.646a.5.5 0 0 1 0 .708L5.707 8l5.647 5.646a.5.5 0 0 1-.708.708l-6-6a.5.5 0 0 1 0-.708l6-6a.5.5 0 0 1 .708 0z"></path></svg></button>
#>     <script data-bslib-sidebar-init>bslib.Sidebar.initCollapsibleAll()</script>
#>   </div>
#> </div>
```
