/**required: csv with user-id, ts, test assignment, result*****/
/*mungeing*/

drop table mock_log

create table mock_log(user_id bigint, ts int, ab varchar(20)
, landing_page varchar(20), converted int)

LOAD DATA LOCAL INFILE 
'/Users/bennywijatno/Downloads/takehome copy.csv' 
INTO TABLE mock_log
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' 
LINES TERMINATED BY '\n';

select  * from mock_log limit 1000

select min(FROM_UNIXTIME(ts)), max(FROM_UNIXTIME(ts)) from mock_log limit 1000
-- '1356998400','1357084799'
-- '2012-12-31 16:00:00','2013-01-01 15:59:59' Pacific time, 8 hours behind UTC

select min(ts), max(ts) from mock_log limit 1000

SELECT * , FROM_UNIXTIME(TS)
FROM MOCK_LOG 
WHERE LANDING_PAGE = 'OLD_PAGE'
AND AB = 'TREATMENT'

SELECT * , FROM_UNIXTIME(TS)
FROM MOCK_LOG 
WHERE LANDING_PAGE = 'NEW_PAGE'
AND AB = 'TREATMENT'

select landing_page, count(distinct user_id) 
from mock_log
group by landing_page

select landing_page, user_id, count(1) from mock_log
group by landing_page, user_id having count(1)>1
-- 'old_page','3003737365','2'

select count(1) from (
  select user_id, count(distinct landing_page) 
	from mock_log
	group by user_id having count(distinct landing_page)>1
) a

-- 2394489
select * from mock_log where user_id=2394489

select count(1) from (
	select user_id, count(distinct ab) 
	from mock_log
	group by user_id having count(distinct ab)>1
) a


select count(1) from mock_log
-- 191148

select count(distinct user_id) from mock_log
-- 186384

-- 4763
select landing_page, count(distinct user_id), sum(converted) 
from mock_log
where user_id in  (
	select user_id 
	from mock_log
	group by user_id having count(distinct landing_page)>1
)
group by landing_page 

-- takehome_cleaned.csv
-- dataset removing users with conflicts
-- data set excluding user-ids with uncertain landing page
select * from mock_log
where user_id not in  (
	select user_id 
	from mock_log
	group by user_id having count(distinct landing_page)>1
) 
order by user_id, ts


-- takehome_cleaned_again.csv
-- dataset assuming treatment and new page is authoritative
select user_id, ab, max(converted) converted
from mock_log
group by user_id, ab

-- takehome_cleaned_again2.csv
-- dataset assuming old page is authoritative when there is a conflict
select user_id, landing_page, max(converted) converted
from mock_log
group by user_id having count(distinct landing_page)=1
union
-- for user ids with 2 landing pages, take old page
select user_id, 'old_page'
, max(converted) 
from mock_log
group by user_id
having count(distinct landing_page) > 1


