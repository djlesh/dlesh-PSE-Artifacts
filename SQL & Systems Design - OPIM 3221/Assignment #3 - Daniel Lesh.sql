SELECT last_name, employee_id, phone_number
FROM EMPLOYEES 
WHERE salary < 3000 ;
 
SELECT last_name, phone_number
FROM EMPLOYEES 
WHERE manager_id = 100 ;

SELECT last_name, salary
FROM EMPLOYEES 
WHERE (salary between 6000 and 7000) or (salary between 8000 and 9000) 
ORDER BY hire_date DESC ;

SELECT last_name, job_id, department_id 
FROM EMPLOYEES 
WHERE last_name like '%s'
ORDER BY hire_date DESC ; 

SELECT last_name, job_id, email 
FROM EMPLOYEES 
WHERE last_name in ('Kochhar', 'De Haan', 'Rajs') ;

SELECT first_name "First Name", commission_pct 
"Commission percentage", salary "Monthly Salary" 
FROM EMPLOYEES 
WHERE commission_pct is not null ; 

SELECT first_name "F Name", last_name "L Name", 
phone_number "Phone #" 
FROM EMPLOYEES 
WHERE hire_date like '%98' ;

SELECT last_name, department_id, salary 
FROM EMPLOYEES
WHERE JOB_ID like 'SA_REP' ; 

SELECT last_name, salary, department_id 
FROM EMPLOYEES
WHERE length(phone_number) = 12
ORDER BY 2 DESC, 3 DESC ;

SELECT last_name, department_id, salary 
FROM EMPLOYEES
WHERE salary < &sal_amount 


