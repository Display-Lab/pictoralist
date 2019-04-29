# Spek generating helpers for testing and development purposes.

#' @title Accept and Promote Candidates
#' @param spek List representation of spek to be modified
#' @param index Integer index(es) of candidates to be annotated as acceptable and promoted
#' @return Modified spek with added attributes
accept_and_promote_candidate <- function(spek, index){
  #spek.candidates[[i]]

}

gen_va_gocc_candidates <- function(){

}

gen_mtx_candidates <- function(){

}

# Spek for va_gocc data with categories documented and not_documented.
gen_va_gocc_spek <- function(){
  list(
    `@id` = "http://example.com/app#example-client",
    `@type` = "http://example.com/slowmo#spek",
    `http://example.com/app#related_location` = list( list(
        `@type` = "http://schema.org/Organization",
        `http://schema.org/address` = list( list(
            `@type` = "http://schema.org/PostalAddress",
            `http://schema.org/addressCountry` = list(list(`@value` = "United States")),
            `http://schema.org/addressLocality` = list(list(`@value` = "NW Washington DC")),
            `http://schema.org/addressRegion` = list(list(`@value` = "Washington DC")),
            `http://schema.org/name` = list(list(`@value` = "U.S. Department of Veterans Affairs")),
            `http://schema.org/postalCode` = list(list(`@value` = "20420")),
            `http://schema.org/streetAddress` = list(list(`@value` = "810 Vermont Avenue"))
          ) ) ) ),
    `http://example.com/slowmo#IsAboutPerformer` = list( list(
        `@id` = "http://example.com/app#4369AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Miles City Community Living Center")) ),
      list(
        `@id` = "http://example.com/app#4609AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Wilmington")) ),
      list(
        `@id` = "http://example.com/app#5039AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Altoona - James E. Van Zandt VA Medical Center")) ),
      list(
        `@id` = "http://example.com/app#5129AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "VA Maryland HCS Baltimore Loch Raven")) ),
      list(
        `@id` = "http://example.com/app#5299AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "VA Butler Healthcare")) ),
      list(
        `@id` = "http://example.com/app#5429AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Coatesville VAMC")) ),
      list(
        `@id` = "http://example.com/app#5509AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "VA Illiana HCS")) ),
      list(
        `@id` = "http://example.com/app#5539AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "John D. Dingell VAMC")) ),
      list(
        `@id` = "http://example.com/app#5549AB",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Eastern Colorado HCS - Pueblo")) ),
      list(
        `@id` = "http://example.com/app#5569AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Captain James A Lovell FHCC")) ),
      list(
        `@id` = "http://example.com/app#5689AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "VA Black Hills - Fort Meade")) ),
      list(
        `@id` = "http://example.com/app#5689AB",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "VA Black Hills - Hot Springs")) ),
      list(
        `@id` = "http://example.com/app#5759AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Grand Junction VAMC")) ),
      list(
        `@id` = "http://example.com/app#5959AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Lebanon VAMC")) ),
      list(
        `@id` = "http://example.com/app#6079AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Madison VA")) ),
      list(
        `@id` = "http://example.com/app#6109AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "VA Northern Indiana HCS - Marion")) ),
      list(
        `@id` = "http://example.com/app#6309AB",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "St. Albans Campus of the VA NY Harbor Healthcare System")) ),
      list(
        `@id` = "http://example.com/app#6359AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Oklahoma City VAMC")) ),
      list(
        `@id` = "http://example.com/app#6429AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Philadelphia VAMC")) ),
      list(
        `@id` = "http://example.com/app#6559AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Aleda E. Lutz VAMC")) ),
      list(
        `@id` = "http://example.com/app#6669AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Sheridan VA Medical Center")) ),
      list(
        `@id` = "http://example.com/app#6939AA",
        `@type` = "http://purl.obolibrary.org/obo/psdo_0000085",
        `http://schema.org/name` = list(list(`@value` = "Wilkes-Barre VAMC")) )
    ),
    `http://example.com/slowmo#InputTable` = list( list(
        `@type` = "http://www.w3.org/ns/csvw#Table",
        `http://www.w3.org/ns/csvw#dialect` = list( list(
            `http://www.w3.org/ns/csvw#delimiter` = list(list(`@value` = ",")),
            `http://www.w3.org/ns/csvw#doubleQuote` = list(list(`@value` = TRUE)),
            `http://www.w3.org/ns/csvw#encoding` = list(list(`@value` = "utf-8")),
            `http://www.w3.org/ns/csvw#header` = list(list(`@value` = TRUE)),
            `http://www.w3.org/ns/csvw#headerRowCount` = list(list(`@value` = "1")),
            `http://www.w3.org/ns/csvw#lineTerminators` = list(list(`@value` = "\\n")),
            `http://www.w3.org/ns/csvw#quoteChar` = list(list(`@value` = "\"")),
            `http://www.w3.org/ns/csvw#skipBlankRows` = list(list(`@value` = TRUE)),
            `http://www.w3.org/ns/csvw#skipColumns` = list(list(`@value` = 0L)),
            `http://www.w3.org/ns/csvw#skipInitialSpace` = list(list(`@value` = FALSE)),
            `http://www.w3.org/ns/csvw#skipRows` = list(list(`@value` = "")),
            `http://www.w3.org/ns/csvw#trim` = list(list(`@value` = FALSE))
          )
        ),
        `http://www.w3.org/ns/csvw#tableSchema` = list(list(
          `http://www.w3.org/ns/csvw#columns` = list(
            list(
              `http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "string")),
              `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "sta6a")),
              `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Site")),
              `http://purl.org/dc/terms/description` = list(list(`@value` = "Performer unique ID")),
              `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "identifier"))
            ),
            list(
              `http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "date")),
              `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "report_month")),
              `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Report Month")),
              `http://purl.org/dc/terms/description` = list(list(`@value` = "Report month")),
              `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "time"))
            ),
            list(
              `http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "integer")),
              `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "documented")),
              `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Documented")),
              `http://purl.org/dc/terms/description` = list(list(`@value` = "Veterans who have been documented")),
              `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "value"))
            ),
            list(
              `http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "integer")),
              `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "not_documented")),
              `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Not Documented")),
              `http://purl.org/dc/terms/description` = list(list(`@value` = "Veterans who have not been documented")),
              `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "value"))
            )
          )
        )),
        `http://purl.org/dc/terms/title` = list(
          list(`@value` = "How many total newly admitted Veterans have documented completing a LST template?")
        )
      )
    ),
    `http://example.com/slowmo#Measure` = list(list(
      `http://purl.org/dc/terms/title` = list(list(`@value` = "Completed LST Template Documentation"))
    )),
    `http://example.com/slowmo#IsAboutCausalPathway` = list(
      list(`@id` = "http://example.com/app#EliminateGap",
           `@type` = "http://example.com/cpo#cpo_0000029")
    ),
    `http://example.com/slowmo#slowmo_0000003` = list(
      list(`@id` = "http://example.com/app#ShowGapTemplate"),
      list(`@id` = "http://example.com/app#ShowTrendTemplate")
    )
  )
}

# Behavior data with perscribing numerators, denominators, and rates.
#  Derived from publicly available NHS perscriber data.
gen_mtx_behavior_spek <- function(){
  list(
    `@id` = "http://example.com/app#nhs-example",
    `@type` = "http://example.com/slowmo#spek",
    `http://example.com/app#related_location` = list( list(
        `@type` = "http://schema.org/Organization",
        `http://schema.org/address` = list( list(
            `@type` = "http://schema.org/PostalAddress",
            `http://schema.org/addressCountry` = list(list(`@value` = "England")),
            `http://schema.org/name` = list(list(`@value` = "NHS General Practitioners"))
          ) ) )
    ),
    `http://example.com/slowmo#InputTable` = list( list(
        `@type` = "http://www.w3.org/ns/csvw#Table",
        `http://www.w3.org/ns/csvw#dialect` = list( list(
            `http://www.w3.org/ns/csvw#delimiter` = list(list(`@value` = ",")),
            `http://www.w3.org/ns/csvw#doubleQuote` = list(list(`@value` = TRUE)),
            `http://www.w3.org/ns/csvw#encoding` = list(list(`@value` = "utf-8")),
            `http://www.w3.org/ns/csvw#header` = list(list(`@value` = TRUE)),
            `http://www.w3.org/ns/csvw#headerRowCount` = list(list(`@value` = "1")),
            `http://www.w3.org/ns/csvw#lineTerminators` = list(list(`@value` = "\\n")),
            `http://www.w3.org/ns/csvw#quoteChar` = list(list(`@value` = "\"")),
            `http://www.w3.org/ns/csvw#skipBlankRows` = list(list(`@value` = TRUE)),
            `http://www.w3.org/ns/csvw#skipColumns` = list(list(`@value` = 0L)),
            `http://www.w3.org/ns/csvw#skipInitialSpace` = list(list(`@value` = FALSE)),
            `http://www.w3.org/ns/csvw#skipRows` = list(list(`@value` = "")),
            `http://www.w3.org/ns/csvw#trim` = list(list(`@value` = FALSE))
          )
        ),
        `http://www.w3.org/ns/csvw#tableSchema` = list(list(
          `http://www.w3.org/ns/csvw#columns` = list( list(
              `http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "string")),
              `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "practice")),
              `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Practice")),
              `http://purl.org/dc/terms/description` = list(list(`@value` = "Practice unique ID")),
              `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "identifier"))
            ),
            list(
              `http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "date")),
              `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "period")),
              `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Period")),
              `http://purl.org/dc/terms/description` = list(list(`@value` = "Month during which mtx was perscribed.")),
              `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "time"))
            ),
            list(
              `http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "integer")),
              `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "total_scripts")),
              `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Number of MTX Perscriptions")),
              `http://purl.org/dc/terms/description` = list(list(`@value` = "Number of mtx perscription scripts written")),
              `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "denominator"))
            ),
            list(
              `http://www.w3.org/ns/csvw#datatype` = list(list(`@value` = "decimal")),
              `http://www.w3.org/ns/csvw#name` = list(list(`@value` = "high_dose_scripts")),
              `http://www.w3.org/ns/csvw#titles` = list(list(`@value` = "Number of High Dose MTX Perscriptions")),
              `http://purl.org/dc/terms/description` = list(list(`@value` = "Number of high dose mtx perscriptions written")),
              `http://example.com/slowmo#ColumnUse` = list(list(`@value` = "numerator"))
            )
          )
        )),
        `http://purl.org/dc/terms/title` = list(list(`@value` = "High dose MTX perscribing."))
      )
    ),
    `http://example.com/slowmo#Measure` = list( list(
        `http://purl.org/dc/terms/title` = list(list(`@value` = "High Dose MethoTrexate Perscribing")),
        `http://example.com/slowmo#goal` = list(list(`@value` = "http://example/com/slomo#achievable_benchmark")
        ) ) )
    )
}
