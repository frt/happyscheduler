import { Component, OnInit, EventEmitter, Output, ViewChild, ElementRef, HostListener } from '@angular/core';
import { FormGroup, FormControl, Validators } from '@angular/forms';
import { Http } from '@angular/http';
import { NotificationsService, SimpleNotificationsComponent } from 'angular2-notifications';

@Component({
    selector: 'app-new-task',
    templateUrl: './new-task.component.html',
    styleUrls: ['./new-task.component.css']
})
export class NewTaskComponent implements OnInit {
    @Output() onNewTask = new EventEmitter();
    newTaskForm;

    @ViewChild('dl')
    private input: ElementRef;

    constructor
        ( private http: Http
        , private notificationsService: NotificationsService
        ) {}

    ngOnInit() {
        this.newTaskForm = new FormGroup({
            name: new FormControl('', Validators.required),
            happy: new FormControl('true', Validators.required),
            deadline: new FormControl('', Validators.pattern('[0-9]{4}-[0-9]{2}-[0-9]{2}')),
            time: new FormControl('', Validators.pattern('[0-9]*'))
        });
    }

    @HostListener('change')
    private missingInputWorkaround() {
        const formCtrl = this.newTaskForm.get('deadline');
        if (this.isBrowserWithoutInputEvent() && this.input.nativeElement.value !== formCtrl.value) {
            formCtrl.setValue(this.input.nativeElement.value);
        }
    }

    private isBrowserWithoutInputEvent() {
	// Firefox for Android (Fennec)
        return /\(Android .+ Firefox\/\d+/i.test(navigator.userAgent);
    }

    onSubmit = function(newTask) {
        newTask.done = false;
        newTask.happy = newTask.happy === 'true';
        newTask.time = newTask.time === '' ? Number(0) : Number(newTask.time);
        newTask.deadline = newTask.deadline ? newTask.deadline : null;
        this.http.post('../tasks', newTask).subscribe(
            (data) => {
                this.newTaskForm.reset();
                this.newTaskForm.controls.happy.setValue('true');
                this.notificationsService.success('new task created!', newTask.name, {
                          timeOut: 5000,
                          pauseOnHover: true,
                          showProgressBar: false
                        });
                this.onNewTask.emit();
            },
            (error) => {
                this.notificationsService.error('An error occurred: ', error, {
                          timeOut: 5000,
                          pauseOnHover: true,
                          showProgressBar: false
                        });
            }
        );
    };
}
