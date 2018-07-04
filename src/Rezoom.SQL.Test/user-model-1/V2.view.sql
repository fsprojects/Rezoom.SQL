create view ViewUsers(Id) as
select Id * 1
from Users
where Name like '%stuff%'
