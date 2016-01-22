--Program zmodyfikowany na projekt przez Damian Darczuk
-- Szkielet programu do zadania z języków programowania
-- Studenci powinni przemianować zadania producentów, konsumentów i bufora
-- Powinni następnie zmienić je tak, by odpowiadały ich własnym zadaniom
-- Powinni także uzupełnić kod o brakujące konstrucje
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Symulacja is
   Liczba_Wyrobow: constant Integer := 5;
   Liczba_Zestawow: constant Integer := 3;
   Liczba_Konsumentow: constant Integer := 2;
   subtype Zakres_Czasu_Produkcji is Integer range 3 .. 6;
   subtype Zakres_Czasu_Konsumpcji is Integer range 4 .. 8;
   subtype Typ_Wyrobow is Integer range 1 .. Liczba_Wyrobow;
   subtype Typ_Zestawow is Integer range 1 .. Liczba_Zestawow;
   subtype Typ_Konsumenta is Integer range 1 .. Liczba_Konsumentow;
   Nazwa_Wyrobu: constant array (Typ_Wyrobow) of String(1 .. 6)
     := ("Ananas", "Serser", "Grzyby", "Salami", "Cebula");
   Nazwa_Zestawu: constant array (Typ_Zestawow) of String(1 .. 7)
     := ("Hawajsk", "Marghar", "Calzone");
   package Losowa_Konsumpcja is new
     Ada.Numerics.Discrete_Random(Zakres_Czasu_Konsumpcji);
   package Losowy_Zestaw is new
     Ada.Numerics.Discrete_Random(Typ_Zestawow);
   type My_Str is new String(1 ..256);

   -- Producent produkuje określony wyrób
   task type Producent is
      -- Nadaj Producentowi tożsamość, czyli rodzaj wyrobu
      entry Zacznij(Wyrob: in Typ_Wyrobow; Czas_Produkcji: in Integer);
   end Producent;


   -- Konsument pobiera z Bufora dowolny zestaw składający się z wyrobów
   task type Konsument is
      -- Nadaj Konsumentowi tożsamość
      entry Zacznij(Numer_Konsumenta: in Typ_Konsumenta;
		    Czas_Konsumpcji: in Integer);
   end Konsument;

   -- W Buforze następuje składanie wyrobów w zestawy
   task type Bufor is
      -- Przyjmij wyrób do magazynu
      entry Przyjmij(Wyrob: in Typ_Wyrobow; Numer: in Integer);
      -- Wydaj zestaw z magazynu
      entry Wydaj(Zestaw: in Typ_Zestawow; Numer: out Integer);
      --przyjmuj zamowienie
      entry Zlorz_Zamowienie(Zestaw: in out Typ_Zestawow; ODP: in out Boolean);
      --sprawdz czy potrzebna dostawa
      entry Czy_Dostawa (Wyrob: in Typ_Wyrobow; Dostawa: out Boolean);
   end Bufor;

   P: array ( 1 .. Liczba_Wyrobow ) of Producent;
   K: array ( 1 .. Liczba_Konsumentow ) of Konsument;
   B: Bufor;

   task body Producent is
      package Losowa_Produkcja is new
	Ada.Numerics.Discrete_Random(Zakres_Czasu_Produkcji);
      G: Losowa_Produkcja.Generator;	--  generator liczb losowych
      Nr_Typu_Wyrobu: Integer;
      Numer_Wyrobu: Integer;
      Produkcja: Integer;
      Dostawa: Boolean;
   begin
      accept Zacznij(Wyrob: in Typ_Wyrobow; Czas_Produkcji: in Integer) do
	 Losowa_Produkcja.Reset(G);	--  zacznij generator liczb losowych
	 Numer_Wyrobu := 1;
	 Nr_Typu_Wyrobu := Wyrob;
	 Produkcja := Czas_Produkcji;
      end Zacznij;
      Put_Line("Podpisano umowe z producentem na skladnik " & Nazwa_Wyrobu(Nr_Typu_Wyrobu));
      loop
         B.Czy_Dostawa(Nr_Typu_Wyrobu, Dostawa);
         if Dostawa = True then
            delay Duration(Losowa_Produkcja.Random(G)); --  symuluj produkcję
            Put_Line("Wyprodukowano sladnik " & Nazwa_Wyrobu(Nr_Typu_Wyrobu)
                     & " numer "  & Integer'Image(Numer_Wyrobu));
            -- Wstaw do magazynu
            B.Przyjmij(Nr_Typu_Wyrobu, Numer_Wyrobu);
            Numer_Wyrobu := Numer_Wyrobu + 1;
         else
            Put_Line("Produkcja skladnika: " & Nazwa_Wyrobu(Nr_Typu_Wyrobu) & " nie potrzebna.");
         end if;
      end loop;
   end Producent;



   task body Konsument is
      G: Losowa_Konsumpcja.Generator;	--  generator liczb losowych (czas)
      G2: Losowy_Zestaw.Generator;	--  też (zestawy)
      Nr_Konsumenta: Typ_Konsumenta;
      Numer_Zestawu: Integer;
      Konsumpcja: Integer;
      Rodzaj_Zestawu: Integer;
      ODP: Boolean;
      Nazwa_Konsumenta: constant array (1 .. Liczba_Konsumentow)
	of String(1 .. 13)
	:= ("GlodnyKlient1", "GlodnyKlient2");
   begin
      accept Zacznij(Numer_Konsumenta: in Typ_Konsumenta;
		     Czas_Konsumpcji: in Integer) do
	 Losowa_Konsumpcja.Reset(G);	--  ustaw generator
	 Losowy_Zestaw.Reset(G2);	--  też
	 Nr_Konsumenta := Numer_Konsumenta;
         Konsumpcja := Czas_Konsumpcji;

      end Zacznij;
      Put_Line("Staly " & Nazwa_Konsumenta(Nr_Konsumenta) & " przyjdzie");
      loop
	 delay Duration(Losowa_Konsumpcja.Random(G)); --  symuluj konsumpcję
	 Rodzaj_Zestawu := Losowy_Zestaw.Random(G2);
	 -- pobierz zestaw do konsumpcji
         ODP:= False;
         Put_Line("Klient " & Nazwa_Konsumenta(Nr_Konsumenta) & " sklada zmowienie");
         while not ODP loop
            Put_Line("Klient " & Nazwa_Konsumenta(Nr_Konsumenta) & " czeka na " & Nazwa_Zestawu(Rodzaj_Zestawu) );
            delay 1.0;
            B.Zlorz_Zamowienie (Rodzaj_Zestawu, ODP);
         end loop;
         B.Wydaj(Rodzaj_Zestawu, Numer_Zestawu);
	 Put_Line(Nazwa_Konsumenta(Nr_Konsumenta) & ": pobrano pizze " &
		    Nazwa_Zestawu(Rodzaj_Zestawu) & " numer " &
		    Integer'Image(Numer_Zestawu));
      end loop;
   end Konsument;

   task body Bufor is
      Pojemnosc_Magazynu: constant Integer := 30;
      Magazyn: array (Typ_Wyrobow) of Integer
	:= (0, 0, 0, 0, 0);
      Sklad_Zestawu: array(Typ_Zestawow, Typ_Wyrobow) of Integer
	:= ((2, 1, 2, 1, 2),
	    (1, 2, 1, 1, 0),
	    (1, 1, 2, 0, 1));
      Numer_Zestawu: array(Typ_Zestawow) of Integer
	:= (1, 1, 1);
      W_Magazynie: Integer := 0;

      function Mozna_Przyjac(Wyrob: Typ_Wyrobow) return Boolean is
	 Wolne: Integer;
      begin
	 if W_Magazynie >= Pojemnosc_Magazynu then
	    return False;
	 else
	    Wolne := Pojemnosc_Magazynu - W_Magazynie;
	    for Z in Typ_Zestawow loop
	      for W in Typ_Wyrobow loop
		if W /= Wyrob then
		 if Magazyn(W) + Wolne - 1 < Sklad_Zestawu(Z, W) then
		    return False;
		 end if;
		end if;
	      end loop;
	    end loop;
	    return True;
	 end if;
      end Mozna_Przyjac;

      function Mozna_Wydac(Zestaw: Typ_Zestawow) return Boolean is
      begin
	 for W in Typ_Wyrobow loop
	    if Magazyn(W) < Sklad_Zestawu(Zestaw, W) then
	       return False;
	    end if;
	 end loop;
	 return True;
      end Mozna_Wydac;

      procedure Sklad_Magazynu is
      begin
	 for W in Typ_Wyrobow loop
            Put_Line("Skl�ad lodowki: " & Integer'Image(Magazyn(W)) & " "
		       & Nazwa_Wyrobu(W));
	 end loop;
      end Sklad_Magazynu;

   begin
      Put_Line("!!!Otwarto Pizzernie!!!");
      loop
         select
            accept Wydaj(Zestaw: in Typ_Zestawow; Numer: out Integer) do
               if Mozna_Wydac(Zestaw) then
                  Put_Line("Wydano pizze " & Nazwa_Zestawu(Zestaw) & " nr " &
                             Integer'Image(Numer_Zestawu(Zestaw)));
                  for W in Typ_Wyrobow loop
                     Magazyn(W) := Magazyn(W) - Sklad_Zestawu(Zestaw, W);
                     W_Magazynie := W_Magazynie - Sklad_Zestawu(Zestaw, W);
                  end loop;
                  Numer := Numer_Zestawu(Zestaw);
                  Numer_Zestawu(Zestaw) := Numer_Zestawu(Zestaw) + 1;
               end if;
            end Wydaj;
         or
            accept Zlorz_Zamowienie (Zestaw: in out Typ_Zestawow; ODP: in out Boolean) do
               if Mozna_Wydac(Zestaw) then
                  ODP := True;
                  Put_Line("Pizza " & Nazwa_Zestawu(Zestaw) & " sie piecze");
               --else
                 -- Put_Line("Klient musi poczekac, az kucharz przygotuje skladniki");
                 -- delay 1.0;
               end if;
            end Zlorz_Zamowienie;
         or
            accept Czy_Dostawa (Wyrob: in Typ_Wyrobow; Dostawa: out Boolean) do
               if Mozna_Przyjac(Wyrob) then
                  Dostawa := True;
               else
                  Dostawa := False;
               end if;
            end Czy_Dostawa;
         or
          --when true =>
            accept Przyjmij(Wyrob: in Typ_Wyrobow; Numer: in Integer) do
               if Mozna_Przyjac(Wyrob) then
                  Put_Line("Przyjeto skladnik " & Nazwa_Wyrobu(Wyrob) & " nr " &
                             Integer'Image(Numer));
                  Magazyn(Wyrob) := Magazyn(Wyrob) + 1;
                  W_Magazynie := W_Magazynie + 1;
               else
                  Put_Line("Odrzucono skladnik " & Nazwa_Wyrobu(Wyrob) & " nr " &
                             Integer'Image(Numer));
               end if;
               Sklad_Magazynu;
            end Przyjmij;
         else
            delay 1.0;
            Put_Line("Brak klientow i zadan, przerwa");
         end select;
      end loop;
   end Bufor;

begin
   --Put_Line("Brak cz�ci dla zestawu ");
   for I in 1 .. Liczba_Wyrobow loop
      P(I).Zacznij(I, 10);
   end loop;
   for J in 1 .. Liczba_Konsumentow loop
      K(J).Zacznij(J,12);
   end loop;
end Symulacja;
--- http://www.adahome.com/rm95/rm9x-A-05-02.html
