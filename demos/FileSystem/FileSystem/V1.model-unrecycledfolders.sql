create view UnrecycledFolders as
    select * from Folders where RecycleItemId is null;
