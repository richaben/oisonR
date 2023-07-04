#' Outil interactif pour tracer une zone (type polygone)
#'
#' @return Lance une fenêtre interactive Shiny pour sélectionner son polygone et
#'  le sauver dans l'environnement pour une future requête
#'  avec la fonction \code{get_taxon_polygon}.
#' @export
#'
#' @importFrom cli cli_alert_success
#' @importFrom geojsonsf geojson_sf
#' @importFrom jsonify to_json
#' @importFrom leaflet leafletOutput renderLeaflet leaflet setView addProviderTiles addTiles WMSTileOptions providerTileOptions addLayersControl layersControlOptions addScaleBar filterNULL
#' @importFrom leaflet.extras addDrawToolbar drawShapeOptions editToolbarOptions selectedPathOptions addSearchOSM searchOptions
#' @importFrom sf st_transform st_as_text
#' @importFrom shiny fluidPage titlePanel actionButton icon br sidebarPanel fluidRow h4 p h5 em observeEvent stopApp shinyApp runApp
#'
#' @examples
#' \dontrun{
#' # -> Lancer l'outil et suivre les instructions
#' outil_selection_zone()
#' }

outil_selection_zone <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel(title = "Outil de s\u00e9lection de zones"),
    leaflet::leafletOutput(
      outputId = "leafmap",
      height = 500,
      width = 800
    ),
    shiny::actionButton(
      "ending",
      "Valider la s\u00e9lection !",
      class = "btn btn-success",
      icon = shiny::icon("map")
    ),
    shiny::br(),
    shiny::br(),
    shiny::sidebarPanel(width = 200,
                        shiny::fluidRow(
                          width = 200,
                          shiny::h4("Comment utiliser cet outil de s\u00e9lection ?"),
                          shiny::p(
                            "1\u00b0) Localiser l\'endroit souhait\u00e9 ! (\u00e0 la souris, avec les +/-, ou avec la recherche );"
                          ),
                          shiny::p(
                            "2\u00b0) S\u00e9lectionner un outil pour tracer la zone (soit polygone ou rectangle);"
                          ),
                          shiny::p("3\u00b0) Editez-le si besoin (optionnel) "),
                          shiny::p(
                            "4\u00b0) Valider pour sauvegarder la s\u00e9lection dans l\'environnement !"
                          ),
                          shiny::h5(
                            shiny::em("Et c\'est pr\u00eat \u00e0 \u00eatre utiliser pour la requ\u00eate OISON")

                          )
                        ))
  )

  server <- function(input, output, session) {

    output$leafmap <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::setView(2.5, 48.8, 6) %>%
        leaflet::addProviderTiles(provider = 'Esri.WorldImagery',
                                  group = "ESRI Orthos") %>%
        leaflet::addProviderTiles(provider = "OpenStreetMap",
                                  group = "Open Street Map") %>%

        leaflet::addTiles(
          "http://wxs.ign.fr/choisirgeoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
          options = c(
            leaflet::WMSTileOptions(tileSize = 256),
            leaflet::providerTileOptions(minZoom = 1, maxZoom = 20)
          ),
          attribution = '<a target=\"_blank\" href=\"https://www.geoportail.gouv.fr/\">Geoportail France</a>',
          group = "IGN Plan"
        ) %>%
        leaflet::addTiles(
          "http://wxs.ign.fr/choisirgeoportail/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
          options = c(
            leaflet::WMSTileOptions(tileSize = 256) ,
            leaflet::providerTileOptions(minZoom = 1, maxZoom = 25)
          ),
          attribution = '<a target=\"_blank\" href=\"https://www.geoportail.gouv.fr/\">Geoportail France</a>',
          group = "IGN G\u00e9oportail"
        ) %>%

        leaflet::addLayersControl(
          overlayGroups = "Zone choisie",
          baseGroups = c(
            "Open Street Map",
            #"Google",
            'IGN Plan',
            "IGN G\u00e9oportail",
            "ESRI Orthos"
          ),
          options = leaflet::layersControlOptions(collapsed = T, position = "topright")
        ) %>%
        leaflet::addScaleBar(position = "bottomleft") %>%
        leaflet.extras::addDrawToolbar(
          singleFeature = T,
          targetGroup = "Zone choisie",
          circleOptions = FALSE,
          polylineOptions = FALSE,
          circleMarkerOptions = FALSE,
          markerOptions = FALSE,
          rectangleOptions = leaflet::filterNULL(
            list(
              shapeOptions = leaflet.extras::drawShapeOptions(
                color = "black",
                fillOpacity = 0.5,
                fillColor = "#2ecc71",
                weight = 1
              ),
              repeatMode = F,
              showArea = T,
              metric = T,
              feet = F,
              nautic = F
            )
          ),
          polygonOptions = leaflet::filterNULL(
            list(
              shapeOptions = leaflet.extras::drawShapeOptions(
                color = "black",
                fillOpacity = 0.5,
                fillColor = "#2ecc71",
                weight = 1
              ),
              repeatMode = F,
              #showRadius = T,
              metric = T,
              feet = F,
              nautic = F
            )
          ),
          editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = leaflet.extras::selectedPathOptions())
        ) %>%
        leaflet.extras::addSearchOSM(
          options = leaflet.extras::searchOptions(
            autoCollapse = TRUE,
            minLength = 2,
            hideMarkerOnCollapse = T
          )
        )
    })

    ## sauvegarde infos dans l'environnement ()
    shiny::observeEvent(input$leafmap_draw_all_features, {
      zone_sel <-
        geojsonsf::geojson_sf(jsonify::to_json(input$leafmap_draw_all_features, unbox = T)) %>%
        sf::st_transform(crs = 2154)

      assign("selection_zone",
             value = zone_sel$geometry %>% sf::st_as_text(),
             envir = globalenv())
    })

    shiny::observeEvent(input$ending, {
      cli::cli_alert_success("Zone(s) sauvegard\u00e9e(s) dans l\'environnement !")
      print(selection_zone)
      shiny::stopApp()
    })

    session$onSessionEnded(function() {
      shiny::stopApp()
    })
  }

  app <- shiny::shinyApp(ui, server)

  shiny::runApp(app, quiet = T, launch.browser = T)
}
