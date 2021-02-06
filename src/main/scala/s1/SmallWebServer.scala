package s1

import org.scalatra._
import scala.collection.mutable.Buffer

class SmallWebServer extends ScalatraServlet {
  
  // Hei, kommentti 24.10.
  
  private var ilmoittautuneet = Buffer[Henkilo]()
  
  private var pizzaBufferi = Buffer[Pizza]()
  
  class Henkilo(val nimi: String) {
    
    //private var omistetutPizzapalat = Buffer[Pizza]()
    
    override def toString = this.nimi
    private var varattuPizza: Option[Pizza] = None
    private var varatutPalat = 0
    
    def varaaPizza(pizza: Pizza, kuinkaMonta: Int) = {
      this.varattuPizza match {
        case Some(pitsa) => 
          if (pitsa == pizza) {
            this.varattuPizza = Some(pizza)
            val varattujenPalojenMaara = pizza.muutaPalojenMaaraa(kuinkaMonta)
            if (kuinkaMonta > 0 && kuinkaMonta <= 6 && varattujenPalojenMaara != 0 ) {
              this.varatutPalat += varattujenPalojenMaara
              "Varaus onnistui! " + this + " varasi pizzasta " + pizza + " " + varattujenPalojenMaara + " palaa!"
            }
            else {
              "Varaus epäonnistui! Tämän pizzan kaikki palat on jo varattu."
            }
          }
          else {
            "Varaus epäonnistui, yksi henkilö voi varata kansanterveydellisistä syistä vain yhdestä pizzasta paloja. \n" +
            this + " voi varata paloja vain pizzasta " + pitsa + "."
          }
        case None => 
          this.varattuPizza = Some(pizza)
          this.varatutPalat += pizza.muutaPalojenMaaraa(kuinkaMonta)
          "Varaus onnistui! " + this + " varasi pizzasta " + pizza + " " + kuinkaMonta + " palaa!"
      }
    }
    
    def paljonMaksaa = {
      this.varattuPizza match {
        case Some(pitsa) =>
          val palaHinta = 1.0/6 * pitsa.hinta
          this.varatutPalat * palaHinta
        case None =>
          0
      }
    }
    
    def varattuPitsa = this.varattuPizza
    
  }
  
  
  class Pizza(val nimi: String, val hinta: Double) {
    
    override def toString = this.nimi
    private var palojenMaara = 6
    
    def kuinkaMontaPalaa = {
      this.palojenMaara
    }
    
    def muutaPalojenMaaraa(kuinkaMonta: Int): Int = { //palauttaa onnistuneesti varattujen palojen määrän
      if (this.palojenMaara - kuinkaMonta >= 0 && kuinkaMonta > 0 && kuinkaMonta <= 6) {
        this.palojenMaara -= kuinkaMonta
        kuinkaMonta
      }
      else {
        0  
      }
    }
    
  }

  
  get("/") {
  <html>
		<head>
			<title>lomake</title>
		</head>

		<body>
			<h1>Laita nimesi pizzalistaan!</h1>
			<p>Pizzalistassa tällä on tällä hetkellä: {ilmoittautuneet.mkString(", ")}</p>
			<p>Lisätyt pizzat: {pizzaBufferi.map( pizza => pizza + " (" + pizza.kuinkaMontaPalaa + "kpl paloja jäljellä)" ).mkString(", ")}</p>
			<form action="/ilmoittaudu" method="post">
  		Nimi:
  		<input type="text" name="nimi"></input>
  		<button type="submit">Lähetä</button>
			</form>

			<form action="/pizzat" method="post">
  		Uuden pizzan nimi:
  		<input type="text" name="pizzanimi"></input>
			Uuden pizzan hinta:
			<input type="number" name="hinta"></input>
  		<button type="submit">Lisää pizza</button>
			</form>
			
			<form action="/varatutslicet" method="post">
			Henkilön nimi (pitää olla jo pizzajonossa):
			<input type="name" name="slicenvaraaja"></input>
			Pizzan nimi (pitää olla jo lisättynä):
			<input type="name" name="mikapizza"></input>
			Monta palaa varataan (voit varata 1-6kpl paloja):
			<input type="number" name="varatutpalat" min="1" max="6"></input>
			<button type="submit">Siirry varaukseen</button>
			</form>

			<h2>Tarkastele loppuunmyydyn pizzan maksujakoa tästä</h2>
			<form action="/tarkasteleloppuunmyytyapizzaa" method="get">
			Pizzan nimi (pitää olla jo lisättynä ja loppuunmyyty):
			<input type="name" name="myytypizza"></input>
			<button type="submit">Tarkastele</button>
			</form>

		</body>
	</html>
}
  
post("/ilmoittaudu") {
    // laitetaan nimi talteen
    ilmoittautuneet += new Henkilo(params("nimi"))
    

    // Ja palautetaan käyttäjälle webbisivu
    <html>
      <head>
				<title>Tervetuloa</title>
			</head>
      <body>
				 <form action="/" method="get">
         <h1>Tervetuloa pizzajonoon {params("nimi")}</h1>
         <p>Pizzajonossa on tällä hetkellä {ilmoittautuneet.size} henkilöä</p>
					Takaisin:
				 <button type="submit">Takaisin</button>
					</form>				
      </body>
    </html>
  }

post("/pizzat") {
    // laitetaan pizza talteen
    pizzaBufferi += new Pizza(params("pizzanimi"), params("hinta").toDouble)
    

    // Ja palautetaan käyttäjälle webbisivu
    <html>
      <head>
				<title>Tervetuloa</title>
			</head>
      <body>
				 <form action="/" method="get">
         <h1>Lisättiin valittaviin pizzoihin {params("pizzanimi")}</h1>
					<p>Siirry takaisin etusivulle varataksesi pizzoista paloja</p>
					Takaisin:
				 <button type="submit">Takaisin</button>
					</form>				
      </body>
    </html>
  }

// tähän for loopilla selvitetään onko henkilö jo listoilla
// filtterimetodi hoitaa homman, ei tartte for -looppia! T toinen Valtteri
post("/varatutslicet") {
    // laitetaan tiedot varauksesta talteen
    if (pizzaBufferi.filter( _.nimi == params("mikapizza")).size > 0) { //testaa onko pizza lisätty jo pizzabufferiin
      val haluttuPizza = pizzaBufferi.filter( _.nimi == params("mikapizza"))(0) //filtteröinnin jälkeen pitäisi olla vain kyseinen haluttu pizzamme, joka saadaan vain indeksillä 0
      if (ilmoittautuneet.filter( _.nimi == params("slicenvaraaja")).size > 0){ //testaa onko henkilön lisätty jo pizzajonoon aka ilmoittautuneet
        val haluttuHenkilo = ilmoittautuneet.filter( _.nimi == params("slicenvaraaja"))(0)
        val returnMessage = haluttuHenkilo.varaaPizza(haluttuPizza, params("varatutpalat").toInt) //kutsuu valitun pizza-olion metodia varaaPaloja ja ottaan palautteen talteen
        
      <html>
      <head>
				<title>Tervetuloa</title>
			</head>
      <body>
				 <form action="/" method="get">
         <h2>{returnMessage}</h2>
         <p>Kaikki valittavina olevat pizzat: {pizzaBufferi.mkString(", ")}</p>
					<p>Siirry takaisin etusivulle varataksesi pizzoista paloja</p>
					Takaisin:
				 <button type="submit">Takaisin</button>
					</form>				
      </body>
    	</html>
      }
      else {
      <html>
      <head>
				<title>Henkilöä ei löydy!</title>
			</head>
      <body>
				<h1>Henkilöä ei löytynyt pizzajonosta, kokeile uudelleen!</h1>
				 <form action="/" method="get">
					Takaisin:
				 <button type="submit">Takaisin</button>
					</form>		
      </body>
    	</html>
        
      }
      
    }
    else {
      <html>
      <head>
				<title>Pizzaa ei löydy</title>
			</head>
      <body>
				<h1>Pizzaa ei löytynyt lisätyistä pizzoista, yritä uudelleen</h1>
				 <form action="/" method="get">
					Takaisin etusivulle:
				 <button type="submit">Takaisin</button>
					</form>		
      </body>
    	</html>
      
    }
  }

  get("/tarkasteleloppuunmyytyapizzaa") {
    // alla oleva if lause tarkastelee onko pizza pizzabufferissa ja onko sen kaikki palat jo varattu
    if (pizzaBufferi.filter( _.nimi == params("myytypizza")).size > 0 && pizzaBufferi.filter( _.nimi == params("myytypizza"))(0).kuinkaMontaPalaa == 0 ) {
       val loppuunmyytyPitsa = pizzaBufferi.filter( _.nimi == params("myytypizza"))(0)
      <html>
      <head>
				<title>loppuunmyytypizza</title>
			</head>
      <body>
				<h1>Tervetuloa loppuunmyydyn pizzan {params("myytypizza")} sivulle</h1>
				<p>Alla on lueteltu kuinka paljon kukin pizzajonossa oleva joutuu pizzasta maksamaan</p>
				<p> {ilmoittautuneet.filter( h => h.varattuPitsa == Some(loppuunmyytyPitsa)).map( h => h + " " + h.paljonMaksaa.toString + "e")mkString(", ")} </p>
				 <form action="/" method="get">
					Takaisin:
				 <button type="submit">Takaisin</button>
					</form>		
      </body>
    	</html>
    }
    else {
     <html>
      <head>
				<title>loppuunmyytypizza</title>
			</head>
      <body>
				<h1>Tätä pizzaa ei ole vielä lisätty tai siitä löytyy vielä varattavia paloja. Kokeile uudelleen</h1>
				 <form action="/" method="get">
					Takaisin:
				 <button type="submit">Takaisin</button>
					</form>		
      </body>
    	</html> 
    }
  }

  


}
