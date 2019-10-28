#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

#include <time.h>
#include <stdlib.h>
#include <math.h>
#include <stdbool.h>
#include <cstdlib>
using namespace std;
const int zakres=40;
string miasto;
	int suma;
	int samodzielne;
	int chodnikowe;
	int kontrapasy;
struct  sciezka
{
	string miasto;
	int suma;
	int samodzielne;
	int chodnikowe;
	int kontrapasy;
//	vector <int> dane;
};

void wczytywanie(vector<sciezka> &tab){	
	ifstream file;
	file.open("proj1.txt");
	if(!file.is_open())
	{
		cout<<"Blad wczytywania";
	}
	else
	{
		cout<<"Dane wczytane"<<endl<<endl;
		string line;
		while(file.good())
		{
			getline(file,line);
			stringstream wiersz(line);
			sciezka pom;
			wiersz>>pom.miasto>>pom.suma>>pom.samodzielne>>pom.chodnikowe>>pom.kontrapasy;
			tab.push_back(pom);
		}
	}
};

void wyswietlanie(vector<sciezka> &tab)
{
	for(int i=0;i<tab.size();i++)
	{
		cout<<tab[i].miasto<<"  ";
		cout<<tab[i].suma<<"  ";
		cout<<tab[i].samodzielne<<"  ";
		cout<<tab[i].chodnikowe<<"  ";
		cout<<tab[i].kontrapasy<<"  "<<endl;
	}	
}
void dodawanie(vector<sciezka> &tab)
{
	sciezka dod;
	string miasto1;
	int suma1;
	int samodzielne1;
	int chodnikowe1;
	int kontrapasy1;
	cout<<"Podaj nazwe miasta dodawanego: ";
	cin>>dod.miasto;
	cout<<"Podaj sume sciezek: ";
	cin>>dod.suma;
	cout<<"Podaj samodzielne: ";
	cin>>dod.samodzielne;
	cout<<"Podaj chodnikowe: ";
	cin>>dod.chodnikowe;
	cout<<"Podaj kontrapasy: ";
	cin>>dod.kontrapasy;	
	tab.push_back(dod);
}
void usuwanie(vector<sciezka> &tab)
{	
	cout<<"Wybierz wiersz, ktory chcesz usunac, (us-1)= ";
	int us;
	cin>>us;
	cout<<"Usunieto miasto "<<tab[us-1].miasto<<endl;
	tab.erase(tab.begin()+us-1); 
	cout<<endl;
	
}
int wyswietlanie_poszczegolnych_elementow(vector<sciezka> &tab)
{
	int i,n, wyswietl, array[34];
	cout<<"Podaj numer wiersza, z ktorego chcesz wyswietlic element (pierwszy wiersz=0, ostatni=33), n= ";
	cin>>n;
	i=n;
	array[i]=tab[i].suma;
	wyswietl=array[i];
		if (array[i+1]<wyswietl)
		{
			wyswietl=array[i];
		}
		cout<<"Laczna dlugosc sciezek w miescie "<<tab[i].miasto<<" wynosi "<<wyswietl<<endl;
}
void bubbleSort1 (vector<sciezka> &tab)
{
bool done ;
sciezka pom;
do 
{
	done=true;
	for ( int i = 0; i < tab.size()-1 ; i ++) 
    {
		if (tab[i+1].suma<tab[i].suma) 
		{
			pom = tab[i+1];
			tab[i+1]=tab[i];
			tab[i]=pom;
			done=false;
		}
	 }
} while (!done) ;
}
void pisz_tab1(vector<sciezka> &tab) 
{
int i;
for(int i=0;i<tab.size();i++)
	{
		cout<<tab[i].miasto<<"  ";
		cout<<tab[i].suma<<"  ";
		cout<<tab[i].samodzielne<<"  ";
		cout<<tab[i].chodnikowe<<"  ";
		cout<<tab[i].kontrapasy<<"  "<<endl;
	}
}
float maksimum(vector<sciezka>&tab)
{
    int i;
    float max;
    max=tab[0].suma;
	for(i=0;i<tab.size();i++)
    { 	
    if(tab[i].suma>max)
    {
        max=tab[i].suma;
    }
	}
    return max;
}

float minimum(vector<sciezka>&tab)
{
    int i;
    float min;
    min=tab[0].suma;
    for(i=0;i<tab.size();i++)
    {
        if(tab[i].suma<min)
        {
            min=tab[i].suma;
        }
    }
    return min;
}
int szybkie(vector<sciezka>&tab,int p,int r) 
{
	
	int x=tab[p].suma; 
	int i=p,j=r,w; 
	while (true) 
{
	while (tab[j].suma>x) 
	j--;
	while (tab[i].suma<x) 
	i++;
	if (i<j) 
{
	w=tab[i].suma;
	tab[i].suma=tab[j].suma;
	tab[j].suma=w;
	i++;
	j--;
}
else 
return j;
}
}
void quicksort(vector<sciezka>&tab,int p,int r) 
{
	int q;
	if(p<r)
{  
	q=szybkie(tab,p,r); 
	quicksort(tab,p,q); 
	quicksort(tab,q+1,r); 
}
}
void sortowanie_przez_wstawianie(vector<sciezka>&tab)
{
     int poom, j;
     for(int i=0;i<tab.size();i++)
     {
             //wstawienie elementu w odpowiednie miejsce
             poom = tab[i].suma; //ten element bêdzie wstawiony w odpowiednie miejsce
             j = i-1;
 
             //przesuwanie elementów wiêkszych od poom
             while(j>=0 && tab[j].suma>poom) 
             {
                        tab[j+1].suma = tab[j].suma; //przesuwanie elementów
                        --j;
             }
             tab[j+1].suma = poom; //wstawienie poom w odpowiednie miejsce
            
     }     
}
int wyszukiwanie_liniowe(vector<sciezka>&tab, float sum)
{
	for(int i=0;i<tab.size();i++)
	{
		if(tab[i].suma==sum)
		{
			return i;
		}
	}
    return -1; //zwracamy -1, gdy nie znajdziemy elementu
}
int wyszukiwanie_binarne(vector<sciezka>&tab)
{
	int srednia,l,p,szuk;
  	while(l<=p)
  		{
    		srednia=(l+p)/2;  
    		if(tab[srednia].suma==szuk)   
      		return srednia;
		    if(tab[srednia].suma>szuk)  
      		p=srednia-1;
    		else
      		l=srednia+1;
  		} 
  	return -1;
}
int main()
{	
	ofstream mojplik;
	ofstream mojplik2;
	ofstream mojplik3;
	sciezka pom;
	sciezka dod;
	sciezka usun;
	vector<sciezka> tab;
	clock_t start, koniec;
	wczytywanie(tab);
	
	int operacja, e, d, delta, delta2, delta3, delta4, delta5, l, p, szuk;
	float sum;
	cout<<"Podaj co chcesz zrobic:"<<endl;
	cout<<"1-dodawanie"<<endl;
	cout<<"2-usuwanie"<<endl;
	cout<<"3-wyswietlanie poszczegolnych elementow"<<endl;
	cout<<"4-wyswietlanie wszystkich elementow"<<endl;
	cout<<"5-maksymalny element"<<endl;
	cout<<"6-minimalny element"<<endl;
	cout<<"7-sortowanie babelkowe (bubbleSort)"<<endl;
	cout<<"8-sortowanie szybkie (quickSort)"<<endl;
	cout<<"9-sortowanie przez wstawianie (quickSort)"<<endl;
	cout<<"10-wyszukiwanie liniowe"<<endl;
	cout<<"11-wyszukiwanie binarne"<<endl;
	cin>>operacja;
	
	switch(operacja)
	{
		case 1:
		dodawanie(tab);
		wyswietlanie(tab);
		break;
		case 2:
		usuwanie(tab);
		wyswietlanie(tab);
		break;
		case 3:
		wyswietlanie_poszczegolnych_elementow(tab);
		break;
		case 4:
		wyswietlanie(tab);
		break;
		case 5:
		e=maksimum(tab);
		cout<<"Maksymalny element to: "<<e<<endl<<endl;
		break;
		case 6:
		d=minimum(tab);
    	cout<<"Minimalny element to: "<<d<<endl<<endl;
    	break;
    	case 7:
    	cout << "Posortowana bubbleSort: " << endl<<endl<<endl;
		start = clock(); // bie¿¹cy czas systemowy w ms
		for(int k=0;k<100000;k++)
		{
			bubbleSort1(tab);
		}
		koniec=clock(); // bie¿¹cy czas systemowy w ms
		pisz_tab1(tab);
		delta=(long)(koniec - start);//czas dzia³añ w ms
    	cout << endl<<"Czas sortowania bubbleSort= " << delta<< endl<<endl;   //Je¿eli blokujemy czas to blokujemy tez //bubbleSort1(tab);
    
    	mojplik3.open("Sortowanie bubbleSort.txt");
    	for (int i=0;i<tab.size();i++)
    	{
    		mojplik3<<tab[i].miasto<<"  ";
			mojplik3<<tab[i].suma<<"  ";
			mojplik3<<tab[i].samodzielne<<"  ";
			mojplik3<<tab[i].chodnikowe<<"  ";
			mojplik3<<tab[i].kontrapasy<<"  "<<endl;
    	}
    	mojplik3<<"Czas sortowania bubbleSort= "<<delta<<endl; 
    	mojplik3.close();
    	break;
    		
    	case 8:
    	
		quicksort(tab,0,tab.size());
		cout<<"Quicksort (sortowanie szybkie): "<<endl;
		for (int i=0;i<tab.size();i++) 
		{	
			cout<<tab[i].miasto<<"  ";
			cout<<tab[i].suma<<"  ";
			cout<<tab[i].samodzielne<<"  ";
			cout<<tab[i].chodnikowe<<"  ";
			cout<<tab[i].kontrapasy<<"  "<<endl;
		}
		start = clock(); // bie¿¹cy czas systemowy w ms
		for(int k=0;k<100000;k++)
		{
			quicksort(tab,0,tab.size()); 
		}
		koniec=clock(); // bie¿¹cy czas systemowy w ms
		delta2=(long)(koniec - start);//czas dzia³añ w ms
	    cout << endl<<"Czas sortowania quicksort (szybkiego)= " << delta2<< endl<<endl;   //Je¿eli blokujemy czas to blokujemy tez //quicksort(tab,0,tab.size());
	     
	    mojplik.open("Sortowanie Szybkie.txt");
	    for (int i=0;i<tab.size();i++)
	    {
	    	//cout<<tab[i].miasto<<" = "<<tab[i].kontrapasy<<endl;
	    	mojplik<<tab[i].miasto<<"  ";
			mojplik<<tab[i].suma<<"  ";
			mojplik<<tab[i].samodzielne<<"  ";
			mojplik<<tab[i].chodnikowe<<"  ";
			mojplik<<tab[i].kontrapasy<<"  "<<endl;
	    }
	    mojplik<<"Czas sortowania quicksort (szybkiego)= " << delta2<<endl;
	    mojplik.close(); 
    	break;
    	case 9:
    	sortowanie_przez_wstawianie(tab);
		cout << "Posortowana sortowanie_przez_wstawianie: " << endl<<endl<<endl;
		for (int i=0;i<tab.size();i++) 
		{	
			cout<<tab[i].miasto<<"  ";
			cout<<tab[i].suma<<"  ";
			cout<<tab[i].samodzielne<<"  ";
			cout<<tab[i].chodnikowe<<"  ";
			cout<<tab[i].kontrapasy<<"  "<<endl;
		}
		
		start = clock(); // bie¿¹cy czas systemowy w ms
		for(int k=0;k<100000;k++)
		{
			sortowanie_przez_wstawianie(tab);
		}
		koniec=clock(); // bie¿¹cy czas systemowy w ms
		
		 delta3=(long)(koniec - start);//czas dzia³añ w ms
	    cout << endl<<"Czas sortowania przez wstawianie= " << delta3<< endl<<endl;   //Je¿eli blokujemy czas to blokujemy tez //sortowanie_przez_wstawianie(tab);
	    mojplik2.open("Sortowanie Przez Wstawianie.txt");
	    sortowanie_przez_wstawianie(tab);
	    for (int i=0;i<tab.size();i++)
	    {
	    	mojplik2<<tab[i].miasto<<"  ";
			mojplik2<<tab[i].suma<<"  ";
			mojplik2<<tab[i].samodzielne<<"  ";
			mojplik2<<tab[i].chodnikowe<<"  ";
			mojplik2<<tab[i].kontrapasy<<"  "<<endl;
	    }
	    mojplik2<<"Czas sortowania przez wstawianie= "<<delta3<<endl;
	    mojplik2.close();
		break;
      		
		case 10:
		wyszukiwanie_liniowe(tab,sum);
		cout<<"Podaj szukana wartosc: ";
		cin>>sum;
		for (int i=0;i<tab.size();i++)
		{
			if(sum==tab[i].suma)
			{
				cout<<tab[i].miasto<<endl;
			}	
		}
	
		start = clock(); // bie¿¹cy czas systemowy w ms
		for(int k=0;k<100000;k++)
		{
			wyszukiwanie_liniowe(tab,sum);
		}
		koniec=clock(); // bie¿¹cy czas systemowy w ms
		
		delta4=(long)(koniec - start);//czas dzia³añ w ms
	    cout << endl<<"Czas wyszukiwania liniowego= " << delta4<< endl<<endl; 
		break;
		
		case 11:
		wyszukiwanie_binarne(tab);
		cout<<"Podaj szukana wartosc: ";
		cin>>szuk;
	
		for (int i=0;i<tab.size();i++)
		{
			if(szuk==tab[i].suma)
			{
				cout<<"Szukana wartosc jest w miescie: "<<tab[i].miasto<<endl;
			}
		}
		start = clock(); // bie¿¹cy czas systemowy w ms
		for(int k=0;k<10000000;k++)
		{
			wyszukiwanie_binarne(tab);
		}
		koniec=clock(); // bie¿¹cy czas systemowy w ms
		
		float delta5=(long)(koniec - start);//czas dzia³añ w ms
	    cout << endl<<"Czas wyszukiwania binarnego= " << delta5<< endl<<endl;
		break;
	}
	
	cout<<endl;
	
	/*cout << "Posortowana bubbleSort: " << endl<<endl<<endl;
	start = clock(); // bie¿¹cy czas systemowy w ms
	for(int k=0;k<100000;k++)
	{
		bubbleSort1(tab);
	}
	koniec=clock(); // bie¿¹cy czas systemowy w ms
	pisz_tab1(tab);
	long delta=(long)(koniec - start);//czas dzia³añ w ms
    cout << endl<<"Czas sortowania bubbleSort= " << delta<< endl<<endl;   //Je¿eli blokujemy czas to blokujemy tez //bubbleSort1(tab);
    
    mojplik3.open("Sortowanie bubbleSort.txt");
    for (int i=0;i<tab.size();i++)
    {
    	mojplik3<<tab[i].miasto<<"  ";
		mojplik3<<tab[i].suma<<"  ";
		mojplik3<<tab[i].samodzielne<<"  ";
		mojplik3<<tab[i].chodnikowe<<"  ";
		mojplik3<<tab[i].kontrapasy<<"  "<<endl;
    }
    mojplik3<<"Czas sortowania bubbleSort= "<<delta<<endl; 
    mojplik3.close();*/
    
/*	quicksort(tab,0,tab.size());
	cout<<"Quicksort (sortowanie szybkie): "<<endl;
	for (int i=0;i<tab.size();i++) 
	{	
		cout<<tab[i].miasto<<"  ";
		cout<<tab[i].suma<<"  ";
		cout<<tab[i].samodzielne<<"  ";
		cout<<tab[i].chodnikowe<<"  ";
		cout<<tab[i].kontrapasy<<"  "<<endl;
	}
	start = clock(); // bie¿¹cy czas systemowy w ms
	for(int k=0;k<100000;k++)
	{
		quicksort(tab,0,tab.size());
	}
	koniec=clock(); // bie¿¹cy czas systemowy w ms
	delta2=(long)(koniec - start);//czas dzia³añ w ms
    cout << endl<<"Czas sortowania quicksort (szybkiego)= " << delta2<< endl<<endl;   //Je¿eli blokujemy czas to blokujemy tez //quicksort(tab,0,tab.size());
     
    mojplik.open("Sortowanie Szybkie.txt");
    for (int i=0;i<tab.size();i++)
    {
    	//cout<<tab[i].miasto<<" = "<<tab[i].kontrapasy<<endl;
    	mojplik<<tab[i].miasto<<"  ";
		mojplik<<tab[i].suma<<"  ";
		mojplik<<tab[i].samodzielne<<"  ";
		mojplik<<tab[i].chodnikowe<<"  ";
		mojplik<<tab[i].kontrapasy<<"  "<<endl;
    }
    mojplik<<"Czas sortowania quicksort (szybkiego)= " << delta2<<endl;
    mojplik.close(); 
  */  
    
    /*
    sortowanie_przez_wstawianie(tab);
	cout << "Posortowana sortowanie_przez_wstawianie: " << endl<<endl<<endl;
	for (int i=0;i<tab.size();i++) 
	{	
		cout<<tab[i].miasto<<"  ";
		cout<<tab[i].suma<<"  ";
		cout<<tab[i].samodzielne<<"  ";
		cout<<tab[i].chodnikowe<<"  ";
		cout<<tab[i].kontrapasy<<"  "<<endl;
	}
	
	start = clock(); // bie¿¹cy czas systemowy w ms
	for(int k=0;k<100000;k++)
	{
		sortowanie_przez_wstawianie(tab);
	}
	koniec=clock(); // bie¿¹cy czas systemowy w ms
	
	long delta3=(long)(koniec - start);//czas dzia³añ w ms
    cout << endl<<"Czas sortowania przez wstawianie= " << delta3<< endl<<endl;   //Je¿eli blokujemy czas to blokujemy tez //sortowanie_przez_wstawianie(tab);
    mojplik2.open("Sortowanie Przez Wstawianie.txt");
    sortowanie_przez_wstawianie(tab);
    for (int i=0;i<tab.size();i++)
    {
    
    	mojplik2<<tab[i].miasto<<"  ";
		mojplik2<<tab[i].suma<<"  ";
		mojplik2<<tab[i].samodzielne<<"  ";
		mojplik2<<tab[i].chodnikowe<<"  ";
		mojplik2<<tab[i].kontrapasy<<"  "<<endl;
    }
    mojplik2<<"Czas sortowania przez wstawianie= "<<delta3<<endl;
    mojplik2.close();
    */
    
	return 0;
}
