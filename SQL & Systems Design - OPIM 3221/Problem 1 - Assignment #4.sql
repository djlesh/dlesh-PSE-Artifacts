SELECT INITCAP(first_name) "Name", LENGTH(first_name) "Length", salary "Salary"
FROM EMPLOYEES
WHERE first_name LIKE 'S%'
OR first_name LIKE 'P%' 
OR first_name LIKE 'M%' 
ORDER BY SALARY DESC ; 


SELECT INITCAP(first_name) "Name", LENGTH(first_name) "Length", salary "Salary"
FROM EMPLOYEES
WHERE first_name LIKE '&start_letter%' 
ORDER BY SALARY DESC ; 


SELECT INITCAP(first_name) "Name", LENGTH(first_name) "Length", salary "Salary"
FROM EMPLOYEES
WHERE first_name LIKE UPPER ('&start_letter%')
ORDER BY SALARY DESC ; 


SELECT employee_id, trunc(months_between(end_date,start_date)/12) "Years"
FROM JOB_HISTORY
WHERE trunc(months_between(end_date,start_date)/12) > 1 ;


SELECT first_name, RPAD(email, 12, '@') "Email Address"
FROM EMPLOYEES ;


SELECT RPAD(first_name, 8)||' '||RPAD(' ', salary/1000+1, '*')
    EMPLOYEES_AND_THEIR_SALARIES
FROM EMPLOYEES 
ORDER BY salary DESC ;


SELECT first_name, TRUNC((SYSDATE-hire_date)/7) AS TENURE, (salary/4) Weekly_Salary
FROM EMPLOYEES 
WHERE department_id = 50 
ORDER BY TENURE ;




























