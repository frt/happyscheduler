import { Component, OnInit } from '@angular/core';
import { FormGroup, FormControl, Validators } from '@angular/forms';
import { Http } from '@angular/http';
import { InfoMessagesService } from '../info-messages.service';

@Component({
    selector: 'app-new-task',
    templateUrl: './new-task.component.html',
    styleUrls: ['./new-task.component.css']
})
export class NewTaskComponent implements OnInit {
    newTaskForm;

    constructor(private http: Http, private messagesService: InfoMessagesService) {}

    resetForm() {
        this.newTaskForm = new FormGroup({
            name: new FormControl('', Validators.required),
            happy: new FormControl('true'),
            deadline: new FormControl('', Validators.compose([
                Validators.required,
                Validators.pattern('[0-9]{4}-[0-9]{2}-[0-9]{2}')
            ])),
            time: new FormControl('', Validators.compose([
                Validators.required,
                Validators.pattern('[0-9]+')
            ]))
        });
    }

    ngOnInit() {
        this.resetForm();
    }

    onSubmit = function(newTask) {
        newTask.done = false;
        newTask.happy = newTask.happy === 'true';
        newTask.time = Number(newTask.time);
        this.http.post('../tasks', newTask).subscribe(
            (data) => {
                this.resetForm();
                this.messagesService.infoMessage = 'new task created! ' + newTask.name;
            },
            error => alert('An error occurred: ' + error)
        );
    };
}
