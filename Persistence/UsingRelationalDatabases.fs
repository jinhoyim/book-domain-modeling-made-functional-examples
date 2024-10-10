module Persistence.UsingRelationalDatabases

type CustomerId = CustomerId of int
type String50 = String50 of string
type Birthdate = Birthdate of System.DateTime

type Customer = {
    CustomerId: CustomerId
    Name: String50
    Birthdate: Birthdate option
}

(*
    CREATE TABLE Customer (
        CustomerId int Not NULL,
        Name NVARCHAR(50) NOT NULL,
        Birthdate DATETIME NULL,
        CONSTRAINT PK_Customer PRIMARY KEY (CustomerId)
    )
*)

type Contact = {
    ContactId: ContactId
    Info: ContactInfo
}
and ContactInfo =
    | Email of EmailAddress
    | Phone of PhoneNumber
and EmailAddress = EmailAddress of string
and PhoneNumber = PhoneNumber of string
and ContactId = ContactId of int
(*
    CREATE TABLE ContactInfo (
        -- shared data
        ContactId int NOT NULL,
        -- case flags
        IsEmail bit NOT NULL,
        IsPhone bit NOT NULL,
        EmailAddress NVARCHAR(100) NULL,
        PhoneNumber NVARCHAR(25) NULL,
        CONSTRAINT PK_ContactInfo PRIMARY KEY (ContactId)
    )
*)

(*
    CREATE TABLE ContactInfo (
        -- shared data
        ContactId int NOT NULL,
        -- case flags
        IsEmail bit NOT NULL,
        IsPhone bit NOT NULL,
        CONSTRAINT PK_ContactInfo PRIMARY KEY (ContactId)
    )
    CREATE TABLE ContactEmail (
        ContactId int NOT NULL,
        EmailAddress NVARCHAR(100) NOT NULL,
        CONSTRAINT PK_ContactEmail PRIMARY KEY (ContactId)
    )
    CREATE TABLE ContactPhone (
        ContactId int NOT NULL,
        PhoneNumber NVARCHAR(25) NOT NULL,
        CONSTRAINT PK_ContactPhone PRIMARY KEY (ContactId)
    )
*)