create view UnrecycledFiles as
    select * from Files where RecycleItemId is null;
