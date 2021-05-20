-- *FROM NEWPVF DATABASE*
--Question 3
select Customer_name 
from Customer_t
where Customer_ID in

	(select Customer_ID
	from Order_t
	where Order_ID in

		(select Order_ID
		from Order_line_t
		where Product_ID in

			(select Product_ID
			from Product_t
			where Product_Name = 'Computer Desk'
			)
		)
	)

-- Question 4
select distinct OT.Order_ID, CT.Customer_name
from Order_t as OT
join Customer_t as CT 
on OT.Customer_ID = CT.Customer_ID
join Order_line_t as OLT 
on OT.Order_ID = OLT.Order_ID 
where Product_ID in

	(select Product_ID
	from Product_t 
	where Product_Name = 'End Table' OR Product_Name = 'Coffee Table')

--Question 5
select Top 1 Customer_name, SUM(Quantity * Unit_Price) as Total_Purchase_Amount
from Order_t as OT
join Customer_t as CT 
on OT.Customer_ID = CT.Customer_ID
join Order_line_t as OLT 
on OT.Order_ID = OLT.Order_ID 
join Product_t as PT 
on OLT.Product_ID = PT.Product_ID
Where Order_Date between '1/1/2011' and '12/31/2011' 
Group by Customer_name
Order by Total_Purchase_Amount Desc

--Question 6
select RMT.Material_ID, Material_description, VT.Vendor_ID, Vendor_name, ST.Unit_price 
from Vendor_t as VT 
join Supplies_t as ST
on VT.Vendor_ID = ST.Vendor_ID
join Raw_Materials_t as RMT
on ST.Material_ID = RMT.Material_ID
where ST.Unit_price in
	(select Top 2 ST2.Unit_price
	from Supplies_t as ST2 
	join Raw_Materials_t as RMT2
	on ST2.Material_ID = RMT2.Material_ID
	order by ST2.Unit_price Asc)
Order by Unit_price, Material_ID 

-- *FROM HCCDB DATABASE*
--Question 7
select Top 2 *
from BizRule

select Top 2 HccCode
from Hcc

select Top 2 ID, Name
from Patient

select Top 2 *
from PatientHcc

--Except Command: Values in this table will present data that's NOT present in the other table
-- Attempt 1
select PH.PatientID, Name, HccCode
from Patient as P
join PatientHcc as PH
on P.ID = PH.PatientID
Except
(
select PatientID, Name, MainHccCode
from Patient as P
join PatientHcc as PH
on P.ID = PH.PatientID
join BizRule as BR
on PH.HccCode = BR.HccCode
)

-- Attempt 2; this gives the correct output
select PH.PatientID, Name, HccCode
from Patient as P
join PatientHcc as PH
on P.ID = PH.PatientID
Except
    (
    select PH.PatientID, P.Name, BR.HccCode
    from Patient as P
    join PatientHcc as PH
    on P.ID = PH.PatientID
    join BizRule as BR
    on PH.HccCode = BR.MainHccCode
    )


