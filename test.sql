drop table if exists zadanie cascade;
drop sequence if exists id_zadania cascade;
create sequence id_zadania;
create table zadanie (
  idz integer primary key default nextval('id_zadania'),
  tresc text,
  status int check (status between 0 and 2) default 0,
  czasUtw timestamp default now()
);

drop table if exists uzytkownik cascade;
drop sequence if exists id_uzytkownika cascade;
create sequence id_uzytkownika;
create table uzytkownik (
  idu integer primary key default nextval('id_uzytkownika') ,
  nazwisko varchar(30),
  imie varchar(19),
  dataRej timestamp default now(),
  rola char(1) check (rola = 'a' or rola = 's' or rola = 'g')
);

drop table if exists rozwiazanie cascade;
create table rozwiazanie (
  idz integer references zadanie check (idz is not null),
  idu integer references uzytkownik check (idu is not null),
  czasZgl timestamp default now(),
  wynik integer
);
