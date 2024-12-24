-- Step 1: 創建 DentalClinic 資料庫
IF NOT EXISTS (SELECT name FROM sys.databases WHERE name = 'DentalClinic')
BEGIN
    CREATE DATABASE DentalClinic;
END;
GO

-- 使用 DentalClinic 資料庫
USE DentalClinic;
GO

-- Step 2: 清空資料表內容
IF OBJECT_ID('Appointments', 'U') IS NOT NULL
DROP TABLE Appointments;

IF OBJECT_ID('Patients', 'U') IS NOT NULL
DROP TABLE Patients;

IF OBJECT_ID('Dentists', 'U') IS NOT NULL
DROP TABLE Dentists;
GO

-- Step 3: 創建 Patients 資料表
CREATE TABLE Patients (
    PatientID INT IDENTITY(1,1) PRIMARY KEY,
    FirstName VARCHAR(50) NOT NULL,
    LastName VARCHAR(50) NOT NULL,
    DateOfBirth DATE,
    PhoneNumber VARCHAR(20),
    Email VARCHAR(100),
    Address VARCHAR(255),
    Gender CHAR(1) CHECK (Gender IN ('M', 'F')),
    MedicalHistory TEXT
);
GO

-- Step 4: 創建 Dentists 資料表
CREATE TABLE Dentists (
    DentistID INT IDENTITY(1,1) PRIMARY KEY,
    FirstName VARCHAR(50) NOT NULL,
    LastName VARCHAR(50) NOT NULL,
    Specialization VARCHAR(100),
    PhoneNumber VARCHAR(20),
    Email VARCHAR(100)
);
GO

-- Step 5: 創建 Appointments 資料表
CREATE TABLE Appointments (
    AppointmentID INT IDENTITY(1,1) PRIMARY KEY,
    PatientID INT,
    DentistID INT,
    AppointmentDate DATETIME,
    Reason TEXT,
    Status VARCHAR(20) CHECK (Status IN ('Scheduled', 'Completed', 'Cancelled')),
    FOREIGN KEY (PatientID) REFERENCES Patients(PatientID),
    FOREIGN KEY (DentistID) REFERENCES Dentists(DentistID)
);
GO

-- Step 6: 插入測試數據
INSERT INTO Patients (FirstName, LastName, DateOfBirth, PhoneNumber, Email, Address, Gender, MedicalHistory)
VALUES
('John', 'Doe', '1980-04-15', '555-1234', 'johndoe@example.com', '123 Elm St, Springfield', 'M', 'No major medical issues'),
('Jane', 'Smith', '1992-06-25', '555-2345', 'janesmith@example.com', '456 Oak St, Springfield', 'F', 'Asthma'),
('Michael', 'Johnson', '1975-11-05', '555-3456', 'michaeljohnson@example.com', '789 Pine St, Springfield', 'M', 'High blood pressure'),
('Emily', 'Davis', '1990-08-30', '555-4567', 'emilydavis@example.com', '123 Maple St, Springfield', 'F', 'Diabetes'),
('David', 'Martinez', '1985-01-12', '555-5678', 'davidmartinez@example.com', '321 Birch St, Springfield', 'M', 'No significant health history');
GO

INSERT INTO Dentists (FirstName, LastName, Specialization, PhoneNumber, Email)
VALUES
('Alice', 'Brown', 'Orthodontist', '555-6789', 'alicebrown@example.com'),
('Bob', 'Taylor', 'Periodontist', '555-7890', 'bobtaylor@example.com'),
('Charlie', 'Wilson', 'Endodontist', '555-8901', 'charliewilson@example.com'),
('Diana', 'Harris', 'General Dentist', '555-9012', 'dianaharris@example.com'),
('Ethan', 'Clark', 'Pediatric Dentist', '555-0123', 'ethanclark@example.com');
GO

-- 插入預約資料
INSERT INTO Appointments (PatientID, DentistID, AppointmentDate, Reason, Status)
VALUES
(1, 1, '2024-12-28 10:00:00', 'Routine checkup', 'Scheduled'), 
(2, 2, '2024-12-29 11:00:00', 'Periodontal treatment', 'Scheduled'),
(3, 3, '2025-01-02 14:00:00', 'Root canal treatment', 'Scheduled'),
(4, 4, '2024-12-30 09:00:00', 'General dental checkup', 'Scheduled'),
(5, 5, '2024-12-31 13:00:00', 'Pediatric consultation', 'Scheduled');
GO

-- Step 7: 創建視圖
IF OBJECT_ID('PatientAppointmentsOverview', 'V') IS NOT NULL
DROP VIEW PatientAppointmentsOverview;

IF OBJECT_ID('DentistWorkloadSummary', 'V') IS NOT NULL
DROP VIEW DentistWorkloadSummary;

IF OBJECT_ID('UpcomingAppointments', 'V') IS NOT NULL
DROP VIEW UpcomingAppointments;
GO

-- 創建視圖
CREATE VIEW PatientAppointmentsOverview AS
SELECT
    P.PatientID,
    CONCAT(P.FirstName, ' ', P.LastName) AS PatientName,
    P.Gender,
    P.PhoneNumber,
    P.Email,
    A.AppointmentDate,
    A.Reason,
    A.Status
FROM
    Patients AS P
LEFT JOIN
    Appointments AS A ON P.PatientID = A.PatientID;
GO

CREATE VIEW DentistWorkloadSummary AS
SELECT
    D.DentistID,
    CONCAT(D.FirstName, ' ', D.LastName) AS DentistName,
    D.Specialization,
    COUNT(A.AppointmentID) AS TotalAppointments,
    SUM(CASE WHEN A.Status = 'Scheduled' THEN 1 ELSE 0 END) AS ScheduledAppointments,
    SUM(CASE WHEN A.Status = 'Completed' THEN 1 ELSE 0 END) AS CompletedAppointments,
    SUM(CASE WHEN A.Status = 'Cancelled' THEN 1 ELSE 0 END) AS CancelledAppointments
FROM
    Dentists AS D
LEFT JOIN
    Appointments AS A ON D.DentistID = A.DentistID
GROUP BY
    D.DentistID, D.FirstName, D.LastName, D.Specialization;
GO

CREATE VIEW UpcomingAppointments AS
SELECT
    A.AppointmentID,
    A.AppointmentDate,
    CONCAT(P.FirstName, ' ', P.LastName) AS PatientName,
    P.PhoneNumber AS PatientPhone,
    CONCAT(D.FirstName, ' ', D.LastName) AS DentistName,
    D.PhoneNumber AS DentistPhone,
    A.Reason,
    A.Status
FROM
    Appointments AS A
JOIN
    Patients AS P ON A.PatientID = P.PatientID
JOIN
    Dentists AS D ON A.DentistID = D.DentistID
WHERE
    A.AppointmentDate BETWEEN GETDATE() AND DATEADD(DAY, 7, GETDATE())
    AND A.Status = 'Scheduled';
GO

-- Step 7: 測試視圖查詢
-- 查詢特定病患的預約
SELECT * FROM PatientAppointmentsOverview WHERE PatientName = 'John Doe';

-- 獲取醫生的工作量概覽
SELECT * FROM DentistWorkloadSummary WHERE Specialization = 'Orthodontist';

-- 查詢未來 7 天內的所有預約
SELECT * FROM UpcomingAppointments;
