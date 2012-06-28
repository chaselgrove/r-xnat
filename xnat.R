library(RCurl)
library(XML)

have_tcltk <- TRUE
tryCatch(library(tcltk), error=function(e) have_tcltk <<- FALSE)

.subject_search_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<xdat:search allow-diff-columns="0" secure="false"
 xmlns:xdat="http://nrg.wustl.edu/security" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <xdat:root_element_name>xnat:subjectData</xdat:root_element_name>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>PROJECT</xdat:field_ID>
        <xdat:sequence>0</xdat:sequence>
        <xdat:header>Subject label</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>ID</xdat:field_ID>
        <xdat:sequence>1</xdat:sequence>
        <xdat:header>Subject label</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>LABEL</xdat:field_ID>
        <xdat:sequence>1</xdat:sequence>
        <xdat:header>Subject label</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>GENDER_TEXT</xdat:field_ID>
        <xdat:sequence>2</xdat:sequence>
        <xdat:header>Gender</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>HANDEDNESS_TEXT</xdat:field_ID>
        <xdat:sequence>3</xdat:sequence>
        <xdat:header>Handedness</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>DOB</xdat:field_ID>
        <xdat:sequence>4</xdat:sequence>
        <xdat:header>YOB</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>EDUC</xdat:field_ID>
        <xdat:sequence>5</xdat:sequence>
        <xdat:header>Education</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>SES</xdat:field_ID>
        <xdat:sequence>6</xdat:sequence>
        <xdat:header>SES</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>SUB_GROUP</xdat:field_ID>
        <xdat:sequence>7</xdat:sequence>
        <xdat:header>Group</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>RACE</xdat:field_ID>
        <xdat:sequence>8</xdat:sequence>
        <xdat:header>Race</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>xnat:subjectData</xdat:element_name>
        <xdat:field_ID>ETHNICITY</xdat:field_ID>
        <xdat:sequence>9</xdat:sequence>
        <xdat:header>Ethnicity</xdat:header>
    </xdat:search_field>
</xdat:search>'

.get.experiment.search.xml <- function(type)
{

    xml <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<xdat:search ID="" allow-diff-columns="0" secure="false" brief-description="MR Sessions"
 xmlns:xdat="http://nrg.wustl.edu/security" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    <xdat:root_element_name>%s</xdat:root_element_name>
    <xdat:search_field>
        <xdat:element_name>%s</xdat:element_name>
        <xdat:field_ID>LABEL</xdat:field_ID>
        <xdat:sequence>0</xdat:sequence>
        <xdat:type>string</xdat:type>
        <xdat:header>one</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>%s</xdat:element_name>
        <xdat:field_ID>SUBJECT_ID</xdat:field_ID>
        <xdat:sequence>2</xdat:sequence>
        <xdat:type>string</xdat:type>
        <xdat:header>Subject</xdat:header>
    </xdat:search_field>
    <xdat:search_field>
        <xdat:element_name>%s</xdat:element_name>
        <xdat:field_ID>AGE</xdat:field_ID>
        <xdat:sequence>3</xdat:sequence>
        <xdat:type>integer</xdat:type>
        <xdat:header>Age</xdat:header>
    </xdat:search_field>
</xdat:search>', type, type, type, type, type)

    return(xml)

} 

.csv.from.string <- function(string)
{

    c <- textConnection(string)
    csv <- read.csv(c, as.is = TRUE)
    close(c)

    return(csv)

}

xnat.connection <- function(base_url, username=NULL, password=NULL)
{

    .xnat.call <- function(request, customrequest = 'GET', data='') {
        if(is.null(jsid))
            stop('not connected')
        reader <- basicTextGatherer()
        header <- basicTextGatherer()
        if(nchar(data) > 0)
            customrequest = 'POST'
        curlPerform(url = paste(base_url, request, sep = ''), 
                    writefunction = reader$update, 
                    headerfunction = header$update, 
                    customrequest = customrequest, 
                    postfields = data, 
                    ssl.verifypeer = FALSE, 
                    cookie = paste('JSESSIONID=', jsid, sep = ''))
        if(parseHTTPHeader(header$value())['status'] != 200) {
            stop('error during HTTP request')
        }
        return(reader$value())
    }

    close <- function() {
        data <- .xnat.call('/data/JSESSION', customrequest = 'DELETE')
        jsid <<- NULL
    }

    projects <- function() {
        if(is.null(.projects)) {
            data <- .xnat.call('/data/projects?format=csv')
            csv <- .csv.from.string(data)
            csv <- csv[with(csv, order(ID)),]
            rownames(csv) <- 1:nrow(csv)
            .projects <<- csv
        }
        return(.projects)
    }

    subjects <- function(project = NULL) {
        if(is.null(.subjects)) {
            data <- .xnat.call('/data/search?format=csv', 
                               data = .subject_search_xml)
            csv <- .csv.from.string(data)
            names(csv) <- c('project', 
                            'ID', 
                            'subjectid', 
                            'label', 
                            'gender', 
                            'handedness', 
                            'yob', 
                            'education', 
                            'ses', 
                            'group', 
                            'race', 
                            'ethnicity', 
                            'quarantine_status')
            csv <- subset(csv, select = -c(subjectid, quarantine_status))
            csv <- csv[with(csv, order(project, label)),]
            rownames(csv) <- 1:nrow(csv)
            .subjects <<- csv
        }
        if(!is.null(project)) {
            if(!project %in% projects()$ID) {
                stop(sprintf('unknown project "%s"', project))
            }
            rv <- .subjects[.subjects$project==project,]
            rownames(rv) <- 1:nrow(rv)
            return(rv)
        }
        return(.subjects)
    }

    .get.experiment.types <- function(project = NULL, subject = NULL) {
        if(is.null(.experiment.types)) {
            # we're getting searchable types here, which may not be complete, 
            # but is better than nothing
            data <- .xnat.call('/data/search/elements?format=csv')
            csv <- .csv.from.string(data)
            et <- grep('^xnat:.*SessionData$', csv$ELEMENT_NAME, value = TRUE)
            if(length(et) == 0) {
                warning('error getting experiment types; falling back on CT, MR, PET, US')
                .experiment.types <<- c('xnat:ctSessionData', 
                                        'xnat:mrSessionData', 
                                        'xnat:petSessionData', 
                                        'xnat:usSessionData')
            } else {
                .experiment.types <<- et
            }
        }
        return(.experiment.types)
    }

    experiments <- function(e_project = NULL, e_subject = NULL) {
        if(is.null(.experiments)) {
            experiments <- NULL
            for(type in .get.experiment.types()) {
                in_data <- .get.experiment.search.xml(type)
                out_data <- .xnat.call('/data/search?format=csv', 
                                       data = in_data)
                csv <- .csv.from.string(out_data)
                if(nrow(csv) > 0) {
                    if(type == 'xnat:mrSessionData') {
                        csv <- subset(csv, select = c(subject_id, 
                                                      session_id, 
                                                      label, 
                                                      age))
                    } else {
                        csv <- subset(csv, select = c(subject_id, 
                                                      expt_id, 
                                                      label, 
                                                      age))
                    }
                    names(csv) <- c('subject_id', 'ID', 'label', 'age')
                    csv$type <- rep(type, nrow(csv))
                    experiments <- rbind(experiments, csv)
                }
            }
        }
        if(is.null(experiments)) {
            .experiments <<- data.frame()
        } else {
            ss <- subset(subjects(), select = c(ID, label, project))
            experiments <- merge(experiments, 
                                 ss, 
                                 by.x = 'subject_id', 
                                 by.y = 'ID')
            experiments <- subset(experiments, select = c(project, 
                                                          label.y, 
                                                          ID, 
                                                          type, 
                                                          label.x, 
                                                          age))
            names(experiments) <- c('project', 
                                    'subject', 
                                    'ID', 
                                    'type', 
                                    'label', 
                                    'age')
            experiments <- experiments[with(experiments, 
                                            order(project,subject,label)),]
            rownames(experiments) <- 1:nrow(experiments)
            .experiments <<- experiments
        }

        if(!is.null(e_project)) {
            if(!e_project %in% projects()$ID) {
                stop(sprintf('unknown project "%s"', e_project))
            }
            if(!is.null(e_subject)) {
                if(!e_subject %in% subjects(e_project)$label) {
                    stop(sprintf('no subject "%s" in project %s', e_subject, e_project))
                rv <- .experiments[.experiments$project==e_project&&.experiments$subject==e_subject,]
                rownames(rv) <- 1:nrow(rv)
                return(rv)
                }
            rv <- .experiments[.experiments$project==project,]
            rownames(rv) <- 1:nrow(rv)
            return(rv)
            }
        }
        return(.experiments)
    }

    run.stored.search <- function(search_id) {
        if(is.null(.saved.searches)) {
            data <- .xnat.call('/data/search/saved?format=csv')
            csv <- .csv.from.string(data)
            .saved.searches <<- csv$id
        }
        if(!search_id %in% .saved.searches) {
            stop(sprintf('unknown stored search "%s"', search_id))
        }
        data <- .xnat.call(paste('/data/search/saved/', 
                                 search_id, 
                                 '/results?format=csv', 
                                 sep = ''))
        csv <- .csv.from.string(data)
        return(csv)
    }

    is.connected <- function() {
        if(is.null(jsid))
            return(FALSE)
        return(TRUE)
    }

    reader <- basicTextGatherer()
    header <- basicTextGatherer()
    if(is.null(username)) {
        curlPerform(url = paste(base_url, '/', sep = ''), 
                    writefunction = reader$update, 
                    headerfunction = header$update, 
                    ssl.verifypeer = FALSE)
        jsid <<- NULL
        for(h in strsplit(header$value(), '\n')[[1]]) {
            if(substring(h, 1, 23) == 'Set-Cookie: JSESSIONID=') {
                jsid <<- strsplit(substring(h, 24), ';')[[1]][[1]]
            }
        }
        if(is.null(jsid)) {
            stop('error starting session')
        }
    } else {
        if(is.null(password)) {
            if(!have_tcltk) {
                stop("can't prompt for password without tcltk")
            }
            tt <- tktoplevel()
            tktitle(tt) <- 'XNAT Password'
            onOK <- function() {
                tkgrab.release(tt)
                tkdestroy(tt)
            }
            lab <- tklabel(tt, text='Password:')
            pwVar <- tclVar()
            pw <- tkentry(tt, textvariable=pwVar, show='*')
            but <- tkbutton(tt, text='OK', command=onOK)
            tkgrid(lab, pw, but)
            tkfocus(tt)
            tkwait.window(tt)
            password <- tclvalue(pwVar)
        }
        curlPerform(url = paste(base_url, '/data/JSESSION', sep = ''), 
                    writefunction = reader$update, 
                    headerfunction = header$update, 
                    ssl.verifypeer = FALSE, 
                    userpwd = paste(username, password, sep = ':'))
        status = parseHTTPHeader(header$value())['status']
        if(status == 401) {
            stop('bad username/password')
        } else if(status != 200) {
            stop('error authenticating')
        }
        jsid <<- reader$value()
    }

    .projects <- NULL
    .subjects <- NULL
    .experiment.types <- NULL
    .experiments <- NULL
    .saved.searches <- NULL

    rv = list(base_url = base_url, 
              close = close, 
              is.connected = is.connected, 
              projects = projects, 
              subjects = subjects, 
              experiments = experiments, 
              run.stored.search = run.stored.search)

    class(rv) <- 'XNATConnection'

    return(rv)

}

# eof
