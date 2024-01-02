CREATE TABLE users(
    email text NOT NULL,
    hashedPassword text NOT NULL,
    firstName text,
    lastName text,
    company text,
    role text NOT NULL
);

ALTER TABLE users
ADD CONSTRAINT pk_users PRIMARY KEY (email);

INSERT INTO users (
    email,
    hashedPassword, 
    firstName,
    lastName,
    company,
    role
) VALUES (
    'cornet.remi@corem.corp',
    '$2a$10$0l3cq8mOClq3ppBzkVLr0OdssC0BOv0rJrwqcf0JxAeydvHeT1Xhi',
    'Remi',
    'Cornet',
    'Corem Corp',
    'ADMIN'
);

INSERT INTO users (
    email,
    hashedPassword, 
    firstName,
    lastName,
    company,
    role
) VALUES (
    'gaston.elgato@corem.corp',
    '$2a$10$3AN4sSQmbWXlkog6OIJjuesZ0cbi9uWd34j9Lx22Izv9faYD.H6qy',
    'Gaston',
    'El Gato',
    'Corem Corp',
    'RECRUITER'
)