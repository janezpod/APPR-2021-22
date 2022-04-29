# 2. faza: Uvoz podatkov

# sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#  Podatki o zgodovini vozenj s kolesi se zacenjo leta 2010. Do vkljucno leta
# 2017 so bili podatki na AWS nalozeni letno, poznejsi pa so nalozeni mesecno.
# Ker podatki do vklucno leta 2017 vsebujejo cetrtletne datoteke se bo izvedel
# zanje enkratni download. Za pozenejse program preveri, ce so ustrezno 
# posodobljeni (in jih po potrebi posodobi) in manjkajoce downloada.

source('lib/funkcije.r', encoding='UTF-8')

PrenosCB(2011, 2022)

ImenaStolpcevCB <- ImenaStolpcev('tripdata')

#  Ko na hitro pregledamo imena stolpcev tabel, se nam zdi, da so stolpci v
# tabelah enako poimenovane od tabele 1 (2011) do vkljucno tabele 52 (202003).
# Stolpci vsebujejo cas voznje, datum zacetka in konca voznje, ime in stevilka
# zacetne in koncne postaje, stevilko kolesa ter tip narocnine.
#
#  Sledi spremeba v poimenovanju stolpcev, ki je dosledna za vse sledece
# podatke. Dodani se tudi stolpci za ID voznje, tip kolesa ter geografsko sirino in
# dolzino zacetne in koncne postaje. Odstranjen je stolpec za cas voznje.
#
#  S spodnjo zanko lahko preverimo, da so stolpci res dosledni s tabelo 1 do
# tabele 52 ter da so dosledni s tabelo 53 do zadnje tabele 73.
#  
# for (i in 1:length(ImenaStolpcevCB)) {
#   if (i <= 52) {
#     if (all(ImenaStolpcevCB[[i]] == ImenaStolpcevCB[[52]])) {
#       ImenaStolpcevCB[[i]] = TRUE
#     }
#   }
#   else {
#     if (all(ImenaStolpcevCB[[i]] == ImenaStolpcevCB[[length(ImenaStolpcevCB)]])) {
#       ImenaStolpcevCB[[i]] = TRUE
#     }
#   }
# }
# all(ImenaStolpcevCB) == TRUE
# ImenaStolpcevCB <- ImenaStolpcev('tripdata')




