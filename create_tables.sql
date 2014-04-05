create table portfolios (
   portfolioid integer primary key,
   algorithm text not null,
   startamount integer not null
   /* other info about the portfolio */
);

create table stocks (
   ticker text primary key,
   company text not null,
   industry text not null
   /* other info about the stock */
);

create table market (
   activity_ts timestamp not null,
   ticker text not null,
   price double not null,
   volume integer not null
);

create table holdings (
   portfolioid integer not null,
   activity_ts timestamp not null,
   ticker text not null,
   numshares integer not null,
   foreign key (portfolioid) references portfolios(portfolioid)
   foreign key (ticker) references stocks(ticker)
   /* ZZZ : decide null/not null cols */
);

create table transactions (
   portfolioid integer not null,
   activity_ts timestamp not null,
   decision text not null,
   ticker text not null,
   numshares integer not null,
   foreign key (portfolioid) references portfolios(portfolioid)
   foreign key (ticker) references stocks(ticker)
   /* ZZZ : decide null/not null cols */
);
