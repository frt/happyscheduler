import { Component, OnInit, EventEmitter, Output } from '@angular/core';
import { FormGroup, FormControl, Validators } from '@angular/forms';
import { Http } from '@angular/http';
import { InfoMessagesService } from '../info-messages.service';

@Component({
    selector: 'app-new-task',
    templateUrl: './new-task.component.html',
    styleUrls: ['./new-task.component.css']
})
export class NewTaskComponent implements OnInit {
    @Output() onNewTask = new EventEmitter();

    newTaskForm;

    constructor(private http: Http, private messagesService: InfoMessagesService) {}

    ngOnInit() {
        this.newTaskForm = new FormGroup({
            name: new FormControl('', Validators.required),
            happy: new FormControl('true', Validators.required),
            deadline: new FormControl('2038-01-20', Validators.compose([
                Validators.required,
                Validators.pattern('[0-9]{4}-[0-9]{2}-[0-9]{2}')
            ])),
            time: new FormControl('', Validators.compose([
                Validators.required,
                Validators.pattern('[0-9]+')
            ]))
        });
    }

    onSubmit = function(newTask) {
        newTask.done = false;
        newTask.happy = newTask.happy === 'true';
        newTask.time = Number(newTask.time);
        this.http.post('../tasks', newTask).subscribe(
            (data) => {
                this.newTaskForm.reset();
                this.newTaskForm.controls.happy.setValue('true');
                this.messagesService.infoMessage = 'new task created! ' + newTask.name;
                this.onNewTask.emit();
            },
            error => alert('An error occurred: ' + error)
        );
    };
}
