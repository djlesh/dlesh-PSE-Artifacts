SELECT TRUNC(MAX(salary),3) "Maximum", 
TRUNC (MIN(salary),3) "Minimum", 
TRUNC (SUM(salary),3) "Sum",
TRUNC (AVG(salary),3) "Average"
FROM EMPLOYEES ;


SELECT department_id, TRUNC(MIN(salary),3) "Minimum", 
TRUNC (MAX(salary),3) "Maximum",
TRUNC (SUM(salary),3) "Sum",
TRUNC (AVG(salary),3) "Average"
FROM EMPLOYEES 
WHERE (department_id IS NOT NULL
    AND department_id > 50) 
GROUP BY department_id ;


SELECT department_id, COUNT(*)
FROM employees 
WHERE manager_id > 100 AND department_id is not null 
GROUP BY department_id ;


SELECT COUNT(DISTINCT manager_ID) "Number of Managers" 
FROM EMPLOYEES
WHERE department_id = 110 ;


SELECT job_id, MAX(salary) - MIN(salary) "Wage Gap" 
FROM EMPLOYEES
GROUP BY job_id ; 


SELECT manager_id, MAX(salary)
FROM EMPLOYEES
WHERE manager_id IS NOT NULL 
GROUP BY manager_id
HAVING MAX (salary) > 9000 
ORDER BY MAX (salary) DESC ; 


























