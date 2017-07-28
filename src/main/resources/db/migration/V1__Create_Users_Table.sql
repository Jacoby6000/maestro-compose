CREATE TABLE users (
        id UUID PRIMARY KEY,
        first_name TEXT NOT NULL,
        last_name TEXT NOT NULL,
        username TEXT UNIQUE NOT NULL,
        password TEXT NOT NULL,
        last_logged_in_on TIMESTAMP WITHOUT TIME ZONE,
        created_on TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

CREATE TABLE classrooms (
        id UUID PRIMARY KEY,
        owner_id UUID NOT NULL REFERENCES users,
        subject TEXT NOT NULL,
        description TEXT NOT NULL,
        starts_on TIMESTAMP WITHOUT TIME ZONE NOT NULL,
        ends_on TIMESTAMP WITHOUT TIME ZONE,
        created_on TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

CREATE TABLE class_registration_codes (
        id UUID PRIMARY KEY,
        classroom_id UUID NOT NULL REFERENCES classrooms,
        user_id UUID REFERENCES users,
        role TEXT NOT NULL,
        code TEXT NOT NULL,
        redeemed_on TIMESTAMP WITHOUT TIME ZONE,
        created_on TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

CREATE TABLE parent_registration_codes (
        id UUID PRIMARY KEY,
        parent_id UUID REFERENCES users,
        student_id UUID NOT NULL REFERENCES users,
        code TEXT NOT NULL,
        redeemed_on TIMESTAMP WITHOUT TIME ZONE,
        created_on TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

CREATE TABLE classroom_students (
        id UUID PRIMARY KEY,
        student_id UUID NOT NULL REFERENCES users,
        classroom_id UUID NOT NULL REFERENCES classrooms,
        created_on TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

CREATE TABLE classroom_teachers (
        id UUID PRIMARY KEY,
        teacher_id UUID NOT NULL REFERENCES users,
        classroom_id UUID NOT NULL REFERENCES classrooms,
        created_on TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

CREATE TABLE parent_children (
        id UUID PRIMARY KEY,
        parent_id UUID NOT NULL REFERENCES users,
        student_id UUID NOT NULL REFERENCES users,
        created_on TIMESTAMP WITHOUT TIME ZONE NOT NULL
);

