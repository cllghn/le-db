library(shinythemes)
library(shiny)
library(RSQLite)
library(igraph)
library(visNetwork)
library(dplyr)
library(dbplyr)
library(stringr)
library(rmarkdown)

get_edges <- function(wanted_pid, n, con) {
  if (!dbExistsTable(conn = con, "person_x_events")) {
    stop("person_x_events table is not found!",
         call. = FALSE)
  }
  # Create a recepticle for return
  edges <- data.frame(stringsAsFactors = FALSE)
  wanted_events <- c()
  # Cycles to go around
  cycles <- n + 1
  # Repeat query as needed
  for (i in seq_len(cycles)) {
    temp <- tbl(con, "person_x_events") %>%
      select(PID, eventId, Primary.Type, UpdateDate) %>%
      filter(PID %in% wanted_pid | eventId %in% wanted_events) %>%
      mutate(cycle = i) %>%
      collect()
    edges <- rbind(edges, temp)
    wanted_pid <- unique(
      c(wanted_pid, temp[['PID']])
    )
    wanted_events <- unique(
      c(wanted_events, temp[['eventId']])
    )
  }
  # Clean nodes ================================================================
  out <- clean_edges(n = n, edges = edges)
  # Transform to one-mode ======================================================
  out <- out %>%
    inner_join(.,
               select(., eventId, PID2 = PID),
               by = "eventId") %>%
    mutate(cycle = NULL,
           title = Primary.Type) %>%
    select(from = PID, to = PID2, everything()) %>%
    filter(from != to) %>%
    distinct()
  # Return edge list ===========================================================
  out
}

clean_edges <- function(n, edges) {
  if (NROW(edges) < 1) {
    return(edges)
  }
  # Generate list of expected nodes in all but the last cycle ==================
  expected <- unique(
    unlist(
      subset(
        edges, cycle < n+1, select = c("PID")
      ),
      use.names = FALSE)
  )
  # Return dataset where last cycle is filtered ================================
  out <- rbind(
    subset(edges,
           cycle < n+1),
    subset(edges,
           cycle >= n+1 & PID %in% expected)
  )
  out
}

get_nodes <- function(edges, con) {
  if (NROW(edges) < 1) {
    nodes <- data.frame(stringsAsFactors = FALSE)
    return(nodes)
  }
  if (!dbExistsTable(conn = con, "person_info")) {
    stop("person_info table is not found!",
         call. = FALSE)
  }
  # Get unique nodes ===========================================================
  wanted <- unique(c(edges[['from']], edges[['to']]))
  # Query table and return =====================================================
  nodes <- tbl(con, "person_info") %>%
    filter(PID %in% wanted) %>%
    select(id = PID, everything()) %>%
    mutate(full_name = paste(Name_Given, Name_Middle, Name_Sur),
           title     = paste("<b>Name:</b>", full_name, "</br>",
                             "<b>Sex:</b>", Sex, "</br>",
                             "<b>Race:</b>", Race, "</br>",
                             "<b>Probation:</b>", Probation, "</br>",
                             "<b>Custody:</b>", Custody, "</br>",
                             "<b>Gang:</b>", Gang, "</br>",
                             "<b>Parole:</b>", Parole, "</br>",
                             "<b>Caution:</b>", Caution, "</br>")) %>%
    collect()
  nodes
}

retrieve_data <- function(pid, n) {
  if (!is.character(pid)) {
    stop("pid provided is not character vector.",
         call. = FALSE)
  }
  if (!is.numeric(n)) {
    stop("n provided is not a number.",
         call. = FALSE)
  }
  # Database ===================================================================
  # Close connection to DB
  on.exit(dbDisconnect(con))
  # Set local connection
  con <- dbConnect(SQLite(), "my-db.sqlite")
  # Prepare data ===============================================================
  wanted_pid <- strsplit(pid, split=',')[[1]]
  # Retrieve data pieces =======================================================
  edges <- get_edges(wanted_pid = wanted_pid,
                     n          = n,
                     con        = con)
  nodes  <- get_nodes(edges = edges,
                      con   = con)
  # Place all data into list ===================================================
  listed_data <- list(
    "edges" = edges,
    "nodes" = nodes
  )
  listed_data
}