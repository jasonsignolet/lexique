#' Répéter les mots françaises plus communs
#'
#' @param debout int: Les mots plus hauts. Si vous utilisez `fin`, ceci le rang
#'    inferieur.
#' @param fin int: Le rang superieur.
#' @param instructions Boolean: Montre les instructions ou non?
#' @export
#' @examples
#' # Les mille mots plus hauts
#' montre_mots(1000)
#'
#' # Les mots de rang 500 à 600
#' montre_mots(500, 600)
#'
#' # Les mille mots plus bas
#' montre_mots(-1000)
montre_mots <- function(debout, fin = NULL, instructions = TRUE) {
  taille <- mots[, .N]

  if (instructions) {
    cat("#########################################################################\n")
    cat("Appuyez sur la touche ENTER pour montre le mot suivant.\n")
    cat("Appuyez sur n'importe quel bouton et puis appuyez sur ENTER pour quitter.\n")
    cat("Appuyez sur la touche '?' et puis appuyez sur ENTER pour quitter.\n")
    cat(sprintf("Il y a %s mots dans ce base de donnes\n", taille))
    cat("#########################################################################\n")
    cat("\n")
  }

  if (is.null(fin)) {
    if (debout >= 1) {
      fin <- debout
      debout <- 1
    } else {
      debout <- taille + debout
      fin <- taille
    }
  } else {
    fin <- min(fin, taille)
  }

  if (debout != fin) {
    seq_de_mots <- sample(debout:fin)
  } else {
    seq_de_mots <- debout
  }


  for (i in seq_along(seq_de_mots)) {
    m <- seq_de_mots[i]
    mot_courant <- mots[m, mot]
    n <- readline(prompt = paste0(mot_courant, " "))

    if (n == "?" || grepl("\\?", n)) {
      if (n != "?") {
        mot_courant <- gsub("\\?", "", n)
      }

      print(mots[mot == mot_courant])

      print(
        lexique[lemme == mot_courant,
                .(ortho, genre, nombre, phon, orthosyll, freqfilms2, freqlivres,
                  cgram, infover)]
      )

      ouvrir_browser <- readline(prompt = paste0("Entrez 'oui' pour ouvrir browser: "))

      if (ouvrir_browser == "oui") {
        url <- paste0("http://www.wordreference.com/fren/", mot_courant)
        print(url)
        print(browseURL(url,
                        browser = "/usr/bin/open -a 'Google Chrome'",
                        encodeIfNeeded = TRUE))
      }
      next
    }

    if (grepl("[[:alnum:]]", n)) {
      break
    }

  }
  return("fin")
}
