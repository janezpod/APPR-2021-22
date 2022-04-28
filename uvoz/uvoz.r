# 2. faza: Uvoz podatkov

#  Podatki o zgodovini vozenj s kolesi se zacenjo leta 2010. Do vkljucno leta
# 2017 so bili podatki na AWS nalozeni letno, poznejsi pa so nalozeni mesecno.
# Ker podatki do vklucno leta 2017 vsebujejo cetrtletne datoteke se bo izvedel
# zanje enkratni download. Za pozenejse program preveri, ce so ustrezno 
# posodobljeni (in jih po potrebi posodobi) in manjkajoce downloada.

source('lib/funkcije.r', encoding='UTF-8')

PrenosCB(2011,2021)






















# sl <- locale("sl", decimal_mark=",", grouping_mark=".")

